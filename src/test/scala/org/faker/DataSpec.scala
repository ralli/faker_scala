package org.faker

import org.scalatest.{FunSpec, Matchers}

class DataSpec extends FunSpec with Matchers {
  describe("locales") {
    it("should contain three locales if 'de-at' is given") {
      val data = new Faker.Data("de-at")
      data.locales should equal(List("de-at", "de", "en"))
    }

    it("should contain two keys if 'de' is given") {
      val data = new Faker.Data("de")
      data.locales should equal(List("de", "en"))
    }

    it("should contain one key if 'en' is given") {
      val data = new Faker.Data("en")
      data.locales should equal(List("en"))
    }
  }

  describe("get") {
    it("should return a value from the first matching locale") {
      val data = new Faker.Data("de-at")
      val result = data.get("address.country")
      result should be('defined)
    }

    it("should return a value from the fallback locales if no match found") {
      val data = new Faker.Data("de-at")
      val result = data.get("address.secondary_address")
      result should be('defined)
    }

    it("should return nothing if no match is found in any locale") {
      val data = new Faker.Data("de-at")
      val result = data.get("does.not.exist")
      result should be(None)
    }
  }
}
