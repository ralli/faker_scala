package org.faker

import org.scalatest.{FlatSpec, Matchers}

class BitcoinSpec extends FlatSpec with Matchers {

  "address" should "return valid base58 characters" in {
    Bitcoin.address should fullyMatch regex """[0-9,A-Z,a-z]+"""
  }
}
