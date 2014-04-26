package org.faker

import org.faker.Faker.Finance
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

class FinanceSpec extends FlatSpec with Matchers with FakerBehaviors {
  var types = Table("Type", Finance.CREDIT_CARD_TYPES)

  "creditCardType" should behave like validResult(Finance.creditCard(Finance.CREDIT_CARD_TYPES :_*))

  it should "return valid numbers for all types" in {
    forAll(types) { t: Array[String] =>
      Finance.creditCard(t :_*) should not be empty
    }
  }
}
