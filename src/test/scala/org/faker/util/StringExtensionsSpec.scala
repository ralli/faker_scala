package org.faker.util

import org.scalatest.{Matchers, FlatSpec}

class StringExtensionsSpec extends FlatSpec with Matchers with StringExtensions {
  "gsub(string, string)" should "replace all occurrences of the first argument (regexp) with the second argument" in {
    val s = "X1X2X3"
    val re = "\\d"
    val expected = "XAXAXA"
    val result = s.gsub(re, "A")
    result should equal(expected)
  }

  "gsum(string, function)" should "replace all occurrences with the functions value" in {
    val s = "XaXbXc"
    val re = "([a-z])"
    val expected = "XAXBXC"
    val result = s.gsub(re)(m => m.group(1).toUpperCase())
    result should equal(expected)
  }

  "fixumlauts" should "fix umlauts" in {
    val s = "1ä2ö3ü4ß5Ä6Ö7Ü"
    val expected = "1ae2oe3ue4ss5Ae6Oe7Ue"
    val result = s.fixUmlauts
    result should equal(expected)
  }

  "toSnakeCase" should "convert the string to snake case" in {
    val s = "someString"
    val expected = "some_string"
    s.toSnakeCase should equal(expected)
  }

  "titlelize" should "titlelize the string" in {
    val s = "abc def"
    val expected = "Abc Def"
    s.titlelize should equal(expected)
  }

  "removeNonWordChars" should "remove all non word chars" in {
    val s = "abc 73_a!!"
    val expected = "abc73_a"
    s.removeNonWordChars should equal(expected)
  }
}
