package de.sebbraun.hecate.circe

import io.circe.*
import scala.compiletime.*

import de.sebbraun.hecate.*

private inline def addFields[T, FS](
    inline t: T,
    inline j: JsonObject,
    inline fs: FS
): JsonObject =
  inline fs match
    case f: FieldSchema[T, ft, n] =>
      val j1 = j.add(f.name, summonInline[Encoder[ft]](f.getter(t)))
      addFields[T, n](t, j1, f.next)
    case _: EmptySchema[T] =>
      j

private transparent inline def tupleFromCursor[T](
    inline c: HCursor,
    inline fields: RecordFieldSchema[T]
): Either[DecodingFailure, fields.Tail] =
  inline fields match
    case EmptySchema() => Right(EmptyTuple.asInstanceOf[fields.Tail])
    case fs@FieldSchema(name, getter, next) =>
      tupleFromCursor[T](c, next).flatMap { r =>
        c.downField(name).as[fs.FieldType](using summonInline[Decoder[fs.FieldType]]).map { f =>
          (f *: r).asInstanceOf[fields.Tail]
        }
      }

extension [T, FS <: RecordFieldSchema[T]](inline rs: RecordSchema[T, FS])
  inline def encoder: Encoder[T] =
    Encoder.AsObject.instance { t =>
      addFields(t, JsonObject(), rs.fields)
    }

extension [T, FS <: RecordFieldSchema[T]](inline rs: RecordSchema[T, FS])
  inline def decoder: Decoder[T] =
    Decoder.instance { c =>
      tupleFromCursor(c, rs.fields).map(rs.constructor)
    }

extension [T, FS <: RecordFieldSchema[T]](inline rs: RecordSchema[T, FS])
  inline def codec: Codec[T] =
    Codec.from(rs.decoder, rs.encoder)
