package de.sebbraun.hecate

import scala.quoted.*

private[hecate] trait ConstructionAnalyzer[Q <: Quotes, T: Type]:
  this: HecateSchemas[Q, T] =>

  val q: Q
  private given Q = q
  import q.reflect.*

  val ctxSym =
    TypeRepr.of[Hecate.Ctx[T]].typeSymbol
  val fieldSym = ctxSym.memberMethod("field").head
  val optFieldSym = ctxSym.memberMethod("optField").head

  def extractRecordFromConstruction(
      construction: Term
  ): RecordSchema =
    construction match {
      case TypeApply(Select(x, "$asInstanceOf$"), List(Inferred())) =>
        extractRecordFromConstruction(x)
      case Inlined(_, _, actual) => extractRecordFromConstruction(actual)
      case Lambda(_, consTerm)   =>
        // report.errorAndAbort(s"consTerm = ${consTerm.show(using Printer.TreeStructure)}")
        val fields = extractFields(consTerm)
        val reconstr = (args: Map[String, Term]) => {
          assert(fields.keySet == args.keySet)
          substituteFieldsInTerm(consTerm, args).asExprOf[T]
        }
        RecordSchema(fields, reconstr)
      case _ =>
        report.errorAndAbort(
          s"Top-level form must be a lambda, was ${construction.show(using Printer.TreeStructure)}"
        )
    }

  class FieldSchemaAccumulator
      extends TreeAccumulator[Map[String, FieldSchema]]:
    def foldTree(x: Map[String, FieldSchema], tree: Tree)(
        owner: Symbol
    ): Map[String, FieldSchema] =
      tree match {
        case a @ Apply(f, List(getter, rest*))
            if (f.symbol == fieldSym || f.symbol == optFieldSym) =>
          constructFieldSchema(getter, rest) match {
            case Some(fs) =>
              if (x.contains(fs.name)) {
                report.error(s"Duplicate definition of field ${fs.name}", a.pos)
                x
              } else {
                x + (fs.name -> fs)
              }
            case None => x
          }
        case t => foldOverTree(x, t)(owner)
      }

  def extractFields(t: Term) =
    FieldSchemaAccumulator().foldTree(Map(), t)(Symbol.spliceOwner)

  def constructFieldSchema(
      getter: Term,
      rest: Seq[Term]
  ): Option[FieldSchema] =
    getter match {
      case Inlined(_, _, t) => constructFieldSchema(t, rest)
      case l @ Lambda(List(vd @ ValDef(n, tt, None)), e) =>
        val name = rest
          .flatMap {
            case NamedArg("name", Literal(StringConstant(name))) => Some(name)
            case _                                               => None
          }
          .headOption
          .getOrElse {
            e match {
              case Select(Ident(`n`), fieldName) => fieldName
              case _ =>
                report.errorAndAbort(s"Could not infer field name", getter.pos)
            }
          }
        val ttpe = tt.tpe.dealias
        if (!(ttpe =:= TypeRepr.of[T])) {
          report.error(
            s"Must select from type ${TypeRepr.of[T].show(using Printer.TypeReprShortCode)}",
            getter.pos
          )
          return None
        }

        val ftpe = e.tpe.widen
        Some(
          PureFieldSchema(
            name,
            ftpe,
            (outer: Term) => {
              val applE = ftpe.asType match {
                case '[g] =>
                  val outerE = outer.asExprOf[T]
                  val lExpr = l.asExprOf[T => g]
                  '{ $lExpr($outerE) }
              }
              val appl = applE.asTerm
              Term.betaReduce(appl).getOrElse(appl)
            }
          )
        )
    }

  class FieldSubstituter(private val terms: Map[String, Term]) extends TreeMap {
    override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
      case a @ Apply(f, List(getter, rest*))
          if (f.symbol == fieldSym || f.symbol == optFieldSym) =>
        constructFieldSchema(getter, rest) match {
          case Some(FieldSchema(fname, mkVal)) =>
            val arg = terms
              .get(fname)
              .getOrElse(
                report.errorAndAbort(
                  s"Jsonist internal error: Can't substitute field $fname, no corresponding argument found"
                )
              )
            arg
          case None =>
            report.errorAndAbort(
              s"Looks like a field definition, but could not parse as a field definition",
              a.pos
            )
        }
      case other => super.transformTerm(other)(owner)
    }
  }

  def substituteFieldsInTerm(term: Term, args: Map[String, Term]): Term =
    FieldSubstituter(args).transformTerm(term)(Symbol.spliceOwner)
