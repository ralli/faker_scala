package org.faker

import org.scalatest.{Matchers, FlatSpec}

class InternetSpec extends FlatSpec with Matchers with FakerBehaviors {
  def validEmailAddress(s: String) {
    it should "match a valid email address" in {
      s should fullyMatch regex """^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6}$"""
    }
    it should behave like validResult(s)
  }

  "domainName" should behave like validResult(Internet.domainName)
  it should "match a domain name" in {
    Internet.domainName should fullyMatch regex """^\w+\.\w+$"""
  }

  "userName" should behave like validResult(Internet.userName)
  it should "match a valid user name" in {
    Internet.userName should fullyMatch regex "[a-zA-Z0-9]+[._][a-zA-Z0-9]+"
  }

  "email" should behave like validEmailAddress(Internet.email)

  "freeEmail" should behave like validEmailAddress(Internet.email)

  "safeEmail" should behave like validEmailAddress(Internet.safeEmail)
  it should "contain the substring @example." in {
    Internet.safeEmail should include("@example.")
  }

  "domainWord" should behave like validResult(Internet.domainWord)

  "domainSuffix" should behave like validResult(Internet.domainSuffix)

  "password" should behave like validResult(Internet.password)

  "macaddress" should "match a valid mac address" in {
    Internet.macAddress() should fullyMatch regex "^[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}$"
  }

  "ipV4Address" should "match a valid ip address" in {
    Internet.ipV4Address should fullyMatch regex """^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$"""
  }

  "ipV6Address"  should "match a valid ip address" in {
    Internet.ipV6Address should fullyMatch regex """^[0-9a-f]{1,4}:[0-9a-f]{1,4}:[0-9a-f]{1,4}:[0-9a-f]{1,4}:[0-9a-f]{1,4}:[0-9a-f]{1,4}:[0-9a-f]{1,4}:[0-9a-f]{1,4}$"""
  }
}
