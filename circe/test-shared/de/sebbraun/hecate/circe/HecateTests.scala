package de.sebbraun.hecate.circe

import io.circe.*
import io.circe.syntax.*
import de.sebbraun.hecate.*
import de.sebbraun.hecate.circe.*
import utest.*
import de.sebbraun.hecate.auto.name

import de.sebbraun.hecate.circe
import de.sebbraun.hecate.{RecordSchema, FieldSchema, EmptySchema}
case class ManualSchema(x: Int, y: Long)
object ManualSchema:
  val rs = RecordSchema[ManualSchema].of(
    FieldSchema("x", _.x, FieldSchema("y", _.y, EmptySchema())),
    ManualSchema.apply
  )

  val enc = rs.encoder
  val dec = rs.decoder
  val cod = rs.codec

object AutoSchema:
  val s = auto.autoRecordSchema[ManualSchema](j =>
    ManualSchema(x = j.field(_.x), y = j.field(_.y + 1, name := "z") - 1)
  )
  val cod = s.codec

object EncoderTestSuite extends TestSuite:
  val tests = Tests {
    test("can make a Circe codec from a manual schema") {
      given Codec[ManualSchema] = ManualSchema.cod
      val x = ManualSchema(1, 1)
      val j = x.asJson
      assert(j == Json.obj("x" -> 1.asJson, "y" -> 1.asJson))
      val x1 = j.as[ManualSchema]
      assert(x1 == Right(ManualSchema(1, 1)))
    }

    test("can make a Circe codec from an auto schema") {
      given Codec[ManualSchema] = AutoSchema.cod
      val x = ManualSchema(1, 2)
      val j = x.asJson
      assert(j == Json.obj("x" -> 1.asJson, "z" -> 3.asJson))
      val x1 = j.as[ManualSchema]
      assert(x1 == Right(ManualSchema(1, 2)))
    }
  }
