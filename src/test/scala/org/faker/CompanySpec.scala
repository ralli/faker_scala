package org.faker

import org.scalatest.{FlatSpec, Matchers}

class CompanySpec extends FlatSpec with Matchers with FakerBehaviors {

  "company" should behave like validResult(Company.name)

  "catchPhrase" should behave like validResult(Company.catchPhrase)

  "bs" should behave like validResult(Company.bullshit)

  "dunsNumber" should "match a valid duns number" in {
    Company.dunsNumber should fullyMatch regex """\d{2}-\d{3}-\d{4}"""
  }
}
