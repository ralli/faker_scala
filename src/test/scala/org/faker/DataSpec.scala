package org.faker

import org.scalatest.{FunSpec, Matchers}
import org.faker.Faker.{DataHolder, FakerLocale}

class DataSpec extends FunSpec with Matchers {
  describe("get") {
    it("should return a value from the first matching locale") {
      val result = DataHolder.get("address.country")(FakerLocale.DE_AT)
      result should be('defined)
    }

    it("should return a value from the fallback locales if no match found") {
      val result = DataHolder.get("address.secondary_address")(FakerLocale.DE_AT)
      result should be('defined)
    }

    it("should return nothing if no match is found in any locale") {
      val result = DataHolder.get("does.not.exist")(FakerLocale.DE_AT)
      result should be(None)
    }
  }
}
