package org.faker

import org.scalatest.{FlatSpec, Matchers}
import org.faker.Faker.Business

class BusinessSpec extends FlatSpec with Matchers with FakerBehaviors {
  import Faker.DefaultLocale._

  "creditCardNumber" should behave like validResult(Business.creditCardNumber)

  "creditCardType" should behave like validResult(Business.creditCardType)

  "creditCardExpiryDate" should "be a valid date" in {
    Business.creditCardExpiryDate should not be(null)
  }
}
