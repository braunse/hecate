package de.sebbraun.hecate

import io.circe.Decoder
import io.circe.Encoder
import io.circe.JsonObject

import scala.annotation.compileTimeOnly
import scala.quoted.*

private class Hecate[Q <: Quotes, T: Type](val q: Q)
    extends HecateSchemas[Q, T]
    with ConstructionAnalyzer[Q, T]:
  def x = 1

object Hecate:
  sealed trait Ctx[T]:
    @compileTimeOnly("Only within a jsonist construction")
    def field[F](getter: T => F, name: String | InferMarker = Infer): F = ???

    @compileTimeOnly("Only within a jsonist construction")
    def optField[F](
        getter: T => Option[F],
        name: String | InferMarker = Infer
    ): Option[F] = ???

  inline def codecFor[T](
      inline construction: Ctx[T] => T
  ): Codec[T] =
    ${ codecForImpl[T]('construction) }

  private def codecForImpl[T: Type](construction: Expr[Ctx[T] => T])(using
      q: Quotes
  ): Expr[Codec[T]] =
    import q.reflect.*

    val ist = Hecate[q.type, T](q)
    val r = ist.extractRecordFromConstruction(construction.asTerm)

    val codec = '{
      Codec[T](
        encoder = Encoder.AsObject.instance { it =>
          val o = JsonObject()
          ${ r.addToObject('o, 'it) }
        },
        decoder = Decoder.instance { c =>
          ${ r.parseFromCursor('c) }
        }
      )
    }

    // report.info(codec.asTerm.show(using Printer.TreeCode), construction)

    codec

  opaque type InferMarker = Infer.type
  private case object Infer
