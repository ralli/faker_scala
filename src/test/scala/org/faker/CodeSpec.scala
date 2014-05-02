package org.faker

import org.scalatest.{Matchers, FlatSpec}

class CodeSpec extends FlatSpec with Matchers {
  "isbn(10)" should "return a isbn of length 10" in {
    Code.isbn(10) should have length 10
  }

  "isbn(13)" should "return a isbn of length 13" in {
    Code.isbn(13) should have length 13
  }
}
