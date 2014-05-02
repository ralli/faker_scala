package org.faker

import org.scalatest.{FlatSpec, Matchers}

class PhoneNumberSpec extends FlatSpec with Matchers with FakerBehaviors {

  "phoneNumber" should behave like validResult(PhoneNumber.phoneNumber)

  "cellPhone" should behave like validResult(PhoneNumber.cellPhone)
}
