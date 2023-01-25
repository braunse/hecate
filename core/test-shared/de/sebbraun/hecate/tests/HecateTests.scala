package de.sebbraun.hecate.tests

import de.sebbraun.hecate.{_, given}
import io.circe.Json
import io.circe.syntax.*
import utest.*

object HecateTests extends TestSuite {
  val tests = Tests {
    case class MyTest(x: Int, y: Long, z: String)

    test("canMakeCodec") {
      given codec: Codec[MyTest] = Hecate.codecFor[MyTest] { j =>
        MyTest(j.field(_.x), j.field(_.y), j.field(_.z, name = "a"))
      }

      val testObj = MyTest(1, 2, "3")
      val encoded = testObj.asJson
      val expected = Json.obj(
        "x" -> 1.asJson,
        "y" -> 2.asJson,
        "a" -> "3".asJson
      )
      assert(encoded == expected)

      val decoded = expected.as[MyTest]
      assert(decoded == Right(testObj))
    }
  }
}
