package org.faker

import org.scalatest.{FlatSpec, Matchers}

class BaseSpec extends FlatSpec with Matchers {
  "numerify" should "fill in letters for '#' characters" in {
    new Base {
      val pattern = """^abc\d\(!$""".r
      val string = "abc#(!"
      val result = numerify(string)
      result should fullyMatch regex pattern
    }
  }

  "letterify" should "fill in letters for '?' characters" in {
    new Base {
      val pattern = """^123[A-Z,a-z]\(!$""".r
      val string = "123?(!"
      val result = letterify(string)
      result should fullyMatch regex pattern
    }
  }

  "botherify" should "replace '?' and '#' with letters and digits" in {
    new Base {
      val pattern = """^123[A-Z,a-z]def\d\(!$""".r
      val string = "123?def#(!"
      val result = bothify(string)
      result should fullyMatch regex pattern
    }
  }

  "regexify" should "return a result that matches the given regex" in {
    new Base {
      val pattern = """/^[A-PR-UWYZ0-9][A-HK-Y0-9][AEHMNPRTVXY0-9]?[ABEHMNPRVWXY0-9]? {1,2}[0-9][ABD-HJLN-UW-Z]{2}$/"""
      val matchPattern = pattern.gsub( """^/\^""", "").gsub( """\$/$""", "")
      val result = regexify(pattern)
      result should fullyMatch regex matchPattern
    }
  }
}
