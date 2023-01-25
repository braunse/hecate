package de.sebbraun.hecate

import io.circe.*

import scala.quoted.*
import scala.quoted.runtime.Patterns

private[hecate] trait HecateSchemas[Q <: Quotes, T: Type]:
  val q: Q
  private given Q = q
  import q.reflect.*

  sealed trait FieldSchema {
    val name: String
    val getter: Term => Term
    def fieldType: TypeRepr
    def addToObject(obj: Expr[JsonObject], it: Expr[T]): Expr[JsonObject]
    def parseFromCursor(
        cur: Expr[HCursor],
        andThen: Term
    ): Expr[Either[DecodingFailure, T]]
  }

  object FieldSchema {
    def unapply[T](fs: FieldSchema): (String, Term => Term) =
      (fs.name, fs.getter)
  }

  case class PureFieldSchema(
      name: String,
      fieldType: TypeRepr,
      getter: Term => Term
  ) extends FieldSchema {
    def addToObject(obj: Expr[JsonObject], it: Expr[T]): Expr[JsonObject] =
      val nameE = Expr(name)
      fieldType.asType match {
        case '[f] =>
          val gotten = getter(it.asTerm).asExprOf[f]
          Expr.summon[Encoder[f]] match {
            case Some(encoder) =>
              '{
                $obj.add($nameE, $encoder($gotten))
              }
            case None =>
              report.errorAndAbort(
                s"No Encoder[${fieldType}] in scope"
              )
          }
      }

    def parseFromCursor(
        cur: Expr[HCursor],
        andThen: Term
    ): Expr[Either[DecodingFailure, T]] =
      val nameE = Expr(name)
      fieldType.asType match {
        case '[f] =>
          val andThenE = andThen.asExprOf[f => Either[DecodingFailure, T]]
          Expr.summon[Decoder[f]] match {
            case Some(decoder) =>
              '{
                $cur
                  .downField($nameE)
                  .as[`f`](using $decoder)
                  .flatMap($andThenE)
              }
            case None =>
              report.errorAndAbort(
                s"No Decoder[${fieldType.show(using Printer.TypeReprCode)}] in scope"
              )
          }
      }
  }

  case class OptionalFieldSchema(
      name: String,
      underlyingFieldT: TypeRepr,
      getter: Term => Term
  ) extends FieldSchema {
    val fieldType = TypeRepr.of[Option].appliedTo(underlyingFieldT)

    def addToObject(obj: Expr[JsonObject], it: Expr[T]): Expr[JsonObject] =
      val nameE = Expr(name)
      underlyingFieldT.asType match {
        case '[u] =>
          val gotten = getter(it.asTerm).asExprOf[Option[u]]
          Expr.summon[Encoder[u]] match {
            case Some(encoder) =>
              '{
                $gotten match {
                  case Some(x) => $obj.add($nameE, $encoder(x))
                  case None    => $obj
                }
              }
            case None =>
              report.errorAndAbort(
                s"No Encoder[${underlyingFieldT.show(using Printer.TypeReprCode)} in scope"
              )
          }
      }

    def parseFromCursor(
        cur: Expr[HCursor],
        andThen: Term
    ): Expr[Either[DecodingFailure, T]] =
      val nameE = Expr(name)
      underlyingFieldT.asType match {
        case '[f] =>
          val andThenE =
            andThen.asExprOf[Option[f] => Either[DecodingFailure, T]]
          Expr.summon[Decoder[Option[f]]] match {
            case Some(decoder) =>
              '{
                $cur
                  .downField($nameE)
                  .as[Option[`f`]](using $decoder)
                  .flatMap($andThenE)
              }
            case None =>
              report.errorAndAbort(
                s"No Decoder[${underlyingFieldT.show(using Printer.TypeReprCode)}] in scope"
              )
          }
      }
  }

  case class RecordSchema(
      fields: Map[String, FieldSchema],
      constructor: Map[String, Term] => Expr[T]
  ) {
    def addToObject(obj: Expr[JsonObject], it: Expr[T]): Expr[JsonObject] =
      fields.values.foldLeft(obj) { (o, f) => f.addToObject(o, it) }

    def parseFromCursor(cur: Expr[HCursor]): Expr[Either[DecodingFailure, T]] =
      def mkDecoder(
          fields: List[FieldSchema],
          args: Map[String, Term]
      ): Expr[Either[DecodingFailure, T]] =
        fields match {
          case Nil => '{ Right(${ constructor(args) }) }
          case field :: rest =>
            def next(fval: Expr[Any]) =
              mkDecoder(rest, args + (field.name -> fval.asTerm))

            val nameE = Expr(field.name)
            field.fieldType.asType match {
              case '[f] =>
                Expr.summon[Decoder[f]] match {
                  case Some(decoderE) =>
                    '{
                      $cur.downField($nameE).as[`f`](using $decoderE).flatMap {
                        fval => ${ next('fval) }
                      }
                    }
                  case None =>
                    report.errorAndAbort(
                      s"No Decoder[${field.fieldType.show(using Printer.TypeReprShortCode)}] in scope"
                    )
                }
            }
        }

      mkDecoder(fields.values.toList, Map())
  }

  def makeEncoder(s: RecordSchema): Expr[Encoder[T]] =
    '{
      Encoder.AsObject.instance { it =>
        val j = JsonObject()
        ${ s.addToObject('j, 'it) }
      }
    }

  def makeDecoder(s: RecordSchema): Expr[Decoder[T]] =
    '{
      Decoder.instance { c =>
        ${ s.parseFromCursor('c) }
      }
    }
