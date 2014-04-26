package org.faker

import org.scalatest.{FlatSpec, Matchers}

class AddressSpec extends FlatSpec with Matchers with FakerBehaviors {

  import Faker.Address
  import Faker.DefaultLocale._

  "city" should behave like validResult(Address.city)

  "streetName" should behave like validResult(Address.streetName)

  "streetAddress" should behave like validResult(Address.streetAddress())

  "streetAddress(includeSecondary)" should behave like validResult(Address.streetAddress(includeSecondary = true))

  "secondaryAddress" should behave like validResult(Address.secondaryAddress)

  "buildingNumber" should behave like validResult(Address.buildingNumber)

  "zipCode" should behave like validResult(Address.zipCode)

  "timeZone" should behave like validResult(Address.timeZone)

  "zip" should behave like validResult(Address.zip)

  "postcode" should behave like validResult(Address.postcode)

  "streetSuffix" should behave like validResult(Address.streetSuffix)

  "citySuffix" should behave like validResult(Address.citySuffix)

  "cityPrefix" should behave like validResult(Address.cityPrefix)

  "stateAbbr" should behave like validResult(Address.stateAbbr)

  "state" should behave like validResult(Address.state)

  "country" should behave like validResult(Address.country)

  "latitude" should behave like validResult(Address.latitude)
  it should "be a valid latitude" in {
    val value = Address.latitude.toDouble
    value should be >= -90.0
    value should be < 90.0
  }

  "longitude" should behave like validResult(Address.longitude)
  it should "be a valid longitude" in {
    val value = Address.longitude.toDouble
    value should be >= -90.0
    value should be < 90.0
  }
}
