package org.faker

import org.scalatest.{Matchers, FlatSpec}
import Faker.Number

class NumberSpec extends FlatSpec with Matchers {
  "number" should "return the given number of digits" in {
    Number.number(10) should have length 10
  }

  "decimal" should "return the given number of digits" in {
    Number.decimal(5, 2) should fullyMatch regex """\d{5}\.\d{2}"""
  }

  "digit" should "return a single digit" in {
    Number.digit should fullyMatch regex "\\d"
  }

}
