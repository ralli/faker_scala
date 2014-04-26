package org.faker

import org.scalatest.{Matchers, FlatSpec}

trait FakerBehaviors {
  this: FlatSpec with Matchers =>

  def validResult(value: String) = {
    it should "be non empty" in {
      value should not be empty
    }

    it should "not contain unexpanded values" in {
      value shouldNot include regex """#\{[^}]\}"""
    }

    it should "not contain regular expressions" in {
      value shouldNot include regex """^/[^/]*/$"""
    }
  }
}
