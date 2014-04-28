package org.faker

import org.scalatest.{FlatSpec, Matchers}
import org.faker.Faker.Company

class CompanySpec extends FlatSpec with Matchers with FakerBehaviors {

  "company" should behave like validResult(Company.name)

  "suffix" should behave like validResult(Company.suffix)

  "catchPhrase" should behave like validResult(Company.catchPhrase)

  "bs" should behave like validResult(Company.bs)

  "dunsNumber" should "match a valid duns number" in {
    Company.dunsNumber should fullyMatch regex """\d{2}-\d{3}-\d{4}"""
  }
}
