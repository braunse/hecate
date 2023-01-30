package de.sebbraun.hecate.tests

import utest.*
import de.sebbraun.hecate.auto.*

object SmokeTests extends TestSuite:
  val tests = Tests {
    test("Compiles an auto schema") {
      case class X(x: Int, y: Long)
      val s =
        autoRecordSchema[X] { j => X(j.field(_.x), j.field(_.y, name := "z")) }
    }
  }
