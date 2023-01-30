package de.sebbraun.hecate.auto

import scala.quoted.*
import de.sebbraun.hecate.*
import scala.annotation.compileTimeOnly
import scala.collection.View.Collect
import scala.collection.immutable.SeqMap

private class Analyzer[Q <: Quotes, T: Type](using override val q: Q)
    extends QuotesUtils[Q]
    with CollectFields[Q, T]:
  import q.reflect.*

  object symbols {
    val fieldSchemaType = TypeRepr.of[FieldSchema[_, _, _]].typeSymbol
    val emptySchemaType = TypeRepr.of[EmptySchema[_]].typeSymbol
    val fieldSchemaCompanion = TypeRepr.of[FieldSchema.type].termSymbol
    val fieldSchemaApply = fieldSchemaCompanion.methodMember("apply").head
    val emptySchemaCompanion = TypeRepr.of[EmptySchema.type].termSymbol
    val emptySchemaApply = emptySchemaCompanion.methodMember("apply").head
    val recordSchemaType = TypeRepr.of[RecordSchema[_, _]].typeSymbol
    val recordSchemaCompanion = TypeRepr.of[RecordSchema.type].termSymbol
    val recordSchemaApply = recordSchemaCompanion.methodMember("apply").head
    val recordSchemaConstructor = recordSchemaType.primaryConstructor
  }

  def makeConstructor(
      construction: Term,
      fields: Iterable[(String, FieldMetaData)]
  ): Term =
    construction match
      case Inlined(_, _, t) => makeConstructor(t, fields)
      case Lambda(List(vd @ ValDef(_, _, _)), t)
          if (vd.tpt.tpe =:= TypeRepr.of[Analyzer.Ctx[T]]) =>
        makeConstructor(t, fields)
      case _ =>
        val tupleClassSym = Symbol.requiredClass(s"scala.Tuple${fields.size}")
        val tupleClassTpt =
          Applied(
            TypeTree.ref(tupleClassSym),
            fields.map(m => TypeTree.of(using m._2.tpe.asType)).toList
          )
        Lambda(
          Symbol.spliceOwner,
          tpe = MethodType(List("fields"))(
            paramInfosExp = _ => List(tupleClassTpt.tpe),
            resultTypeExp = _ => TypeRepr.of[T]
          ),
          rhsFn = (owner, params) =>
            params match
              case List(fieldsParam: Term) =>
                val fieldTrees = fields.zipWithIndex.map {
                  case ((name, tpe), idx) =>
                    val elemSym =
                      tupleClassSym.methodMember(s"_${idx + 1}").head
                    name -> Select(fieldsParam, elemSym)
                }.toMap
                substituteFields(fieldTrees, construction)
              case _ => throw IllegalStateException("Params mismatch")
        )

  def mkRecordSchema(
      construction: Expr[Analyzer.Ctx[T] => T]
  ): Expr[RecordSchema[T, ?]] =
    // report.errorAndAbort('{ new RecordSchema[Int, EmptySchema[Int]](EmptySchema(), (_: EmptyTuple) => 1)}.asTerm.show(using Printer.TreeStructure))
    // return '{???}
    val term = mkRecordSchema(simplify(construction.asTerm))
    // report.errorAndAbort(term.show(using Printer.TreeShortCode))
    term.asExprOf[RecordSchema[T, ?]]

  def mkRecordSchema(
      construction: Term
  ): Term =
    construction match
      case Lambda(List(ValDef(n, tt, None)), cons)
          if (tt.tpe =:= TypeRepr.of[Analyzer.Ctx[T]]) =>
        val fields = collectFields(cons)
        val n = fields.size
        val (fstt: TypeTree, ftts: List[TypeTree], fsterm: Term) =
          fields.values.foldRight(
            (
              TypeTree.of[EmptySchema[T]],
              Nil: List[TypeTree],
              Apply(
                TypeApply(
                  Select(
                    Ref(symbols.emptySchemaCompanion),
                    symbols.emptySchemaApply
                  ),
                  List(TypeTree.of[T])
                ),
                List()
              ): Term
            )
          ) { (field, tandf) =>
            val FieldMetaData(name, ftpe, fgetter, fdefault) = field
            val (fstt, ftts, fsterm) = tandf
            ftpe.asType match
              case '[fty] =>
                val ft = TypeTree.of[fty]
                val fstt1 = Applied(
                  TypeTree.ref(symbols.fieldSchemaType),
                  List(TypeTree.of[T], ft, fstt)
                )
                val fsterm1 =
                  Apply(
                    TypeApply(
                      Select(
                        Ref(symbols.fieldSchemaCompanion),
                        symbols.fieldSchemaApply
                      ),
                      List(TypeTree.of[T], ft, fstt)
                    ),
                    List(Literal(StringConstant(name)), fgetter, fsterm)
                  )
                (fstt1, ft :: ftts, fsterm1)
          }

        val tupleSym = Symbol.requiredClass(s"scala.Tuple$n")
        val tupleTt = Applied(TypeTree.ref(tupleSym), ftts)

        val fieldNames = fields.keys.toList

        val constructor = makeConstructor(construction, fields)

        Apply(
          TypeApply(
            Select(
              New(
                Applied(
                  TypeTree.ref(symbols.recordSchemaType),
                  List(TypeTree.of[T], fstt)
                )
              ),
              symbols.recordSchemaConstructor
            ),
            List(TypeTree.of[T], fstt)
          ),
          List(fsterm, constructor)
        )

  class substituteFields private (exprs: Map[String, Term]) extends TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term =
      tree match
        case Apply(f, List(getter, modifiers*)) if (f.symbol == fieldSym) =>
          val meta = fieldMetadata(getter, modifiers)
          val expr = exprs(meta.name)
          expr
        case _ => super.transformTerm(tree)(owner)

  object substituteFields:
    def apply(exprs: Map[String, Term], tree: Term): Term =
      new substituteFields(exprs).transformTerm(tree)(tree.symbol)

object Analyzer:
  private[auto] def mkRecordSchema[T: Type](construction: Expr[Ctx[T] => T])(
      using q: Quotes
  ): Expr[RecordSchema[T, ?]] =
    val analyzer = Analyzer[q.type, T]
    analyzer.mkRecordSchema(construction)

  sealed trait Modifier

  sealed trait Ctx[T]:
    def field[F](getter: T => F, modifiers: Modifier*): F

  transparent inline def autoRecordSchema[T](
      inline construction: Analyzer.Ctx[T] => T
  ): RecordSchema[T, ?] =
    ${ mkRecordSchema[T]('construction) }

object name:
  @compileTimeOnly("Can only be used in a Hecate auto-derivation construction")
  def :=(value: String): Analyzer.Modifier = ???

export Analyzer.autoRecordSchema as autoRecordSchema
