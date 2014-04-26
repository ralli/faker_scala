package org.faker

import org.scalatest.{FlatSpec, Matchers}
import org.faker.Faker.Commerce

class CommerceSpec extends FlatSpec with Matchers with FakerBehaviors {
  import Faker.DefaultLocale._

  "color" should behave like validResult(Commerce.color)

  "department" should behave like validResult(Commerce.department)

  "productName" should behave like validResult(Commerce.productName)

  "price" should "be in the range from 0 to 100" in {
    val price = Commerce.price
    price should be >= 0.0
    price should be < 100.0
  }
}
