package org.faker

import org.scalatest.{Matchers, FunSpec}

class YamlSpec extends FunSpec with Matchers {

  describe("get") {
    it("should return a result for a valid key") {
      val data = new Faker.LocaleData("de")
      val result = data.get("de.faker.name.first_name")
      result should be('defined)
    }

    it("should return nothing for an invalid key") {
      val data = new Faker.LocaleData("de")
      val result = data.get("de.faker.does.not.exist")
      result should be(None)
    }
  }

}
