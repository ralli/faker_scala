package org.faker

import org.scalatest.{FlatSpec, Matchers}
import org.faker.Faker.PhoneNumber

class PhoneNumberSpec extends FlatSpec with Matchers with FakerBehaviors {
  "phoneNumber" should behave like validResult(PhoneNumber.phoneNumber)

  "cellPhone" should behave like validResult(PhoneNumber.cellPhone)
}
