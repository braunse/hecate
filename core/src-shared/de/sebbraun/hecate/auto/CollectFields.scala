package de.sebbraun.hecate.auto

import scala.quoted.Quotes
import scala.quoted.Type
import scala.collection.immutable.SeqMap
import scala.quoted.Expr
import de.sebbraun.hecate.auto.MV.Failed
import scala.collection.immutable.TreeSeqMap

trait CollectFields[Q <: Quotes, T: Type](using val q: Q)
    extends QuotesUtils[Q]:
  import q.reflect.*

  val ctxSym = TypeRepr.of[Analyzer.Ctx[T]].typeSymbol
  val fieldSym = ctxSym.methodMember("field").head

  val nameModSym = TypeRepr.of[name.type].termSymbol.methodMember(":=").head

  case class FieldCollectState(
      name: MV[String, Position],
      tpe: TypeRepr,
      getter: Term,
      default: MV[Term, Position]
  ):
    def finish(errorPos: Option[Position]): FieldMetaData =
      val name1: String = ???
      val default1: Option[Term] = ???
      FieldMetaData(name1, tpe, getter, default1)

  case class FieldMetaData(
      name: String,
      tpe: TypeRepr,
      getter: Term,
      default: Option[Term]
  )

  def fieldMetadata(getter: Term, modifiers: Seq[Term]) =
    getter.tpe.asType match
      case '[T => f] =>
        val tpe = TypeRepr.of[f]
        val initialName = collectPotentialNames(getter)
        val initial = FieldCollectState(initialName, tpe, getter, MV.Unknown)
        val withMods = modifiers.foldLeft(initial)(adaptMetadata)
        val finalName = withMods.name.getOrReportAndAbort(
          "Could not infer name of field",
          "Conflicting names for field",
          Some((n: String) => s"Named as $n here"),
          Some(getter.pos)
        )
        val finalDefault = withMods.default.getOptionOrReportAndAbort(
          "Conflicting defaults for field",
          Some(_ => s"Default value given here"),
          Some(getter.pos)
        )
        FieldMetaData(finalName, tpe, getter, finalDefault)

  def adaptMetadata(
      meta: FieldCollectState,
      mod: Term
  ): FieldCollectState =
    mod match
      case Apply(f, List(nameTerm)) if (f.symbol == nameModSym) =>
        nameTerm.asExprOf[String].value match
          case Some(nameV) =>
            meta.copy(name = meta.name.explicit(nameV, Some(nameTerm.pos)))
          case None =>
            meta.copy(name =
              meta.name
                .fail("Name is not a constant value", Some(nameTerm.pos))
            )
      case Typed(t, _)       => adaptMetadata(meta, t)
      case Repeated(mods, _) => mods.foldLeft(meta)(adaptMetadata)
      case other =>
        report.errorAndAbort(
          s"Unknown modifier expression: ${other.show(using Printer.TreeStructure)}"
        )

  object collectPotentialNames extends TreeAccumulator[MV[String, Position]]:
    override def foldTree(nm: MV[String, Position], tree: Tree)(
        owner: Symbol
    ): MV[String, Position] =
      tree match
        case s @ Select(t, n) if (t.tpe.widen =:= TypeRepr.of[T]) =>
          // report.errorAndAbort(s"Candidate name $n, t.tpe = ${t.tpe.show(using Printer.TypeReprCode)}", s.pos)
          nm.orInfer(n, Some(s.pos))
        case other => foldOverTree(nm, other)(owner)

    def apply(tree: Tree): MV[String, Position] =
      foldOverTree(MV.Unknown, tree)(tree.symbol)

  object collectFields extends TreeAccumulator[SeqMap[String, FieldMetaData]]:
    override def foldTree(x: SeqMap[String, FieldMetaData], tree: Tree)(
        owner: Symbol
    ): SeqMap[String, FieldMetaData] =
      tree match
        case Apply(f, List(getter, modifiers*)) if (f.symbol == fieldSym) =>
          val meta = fieldMetadata(getter, modifiers)
          x.get(meta.name).foreach { fm =>
            report.error(
              s"Previous definition of field ${meta.name}",
              fm.getter.pos
            )
            report.errorAndAbort(
              s"Field ${meta.name} is already defined",
              getter.pos
            )
          }
          x + (meta.name -> meta)
        case _ => super.foldOverTree(x, tree)(owner)

    def apply(tree: Tree): SeqMap[String, FieldMetaData] =
      foldOverTree(TreeSeqMap(), tree)(tree.symbol)
