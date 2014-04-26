package org.faker

import org.scalatest.{FlatSpec, Matchers}
import org.faker.Faker.FakerLocale

class FakerLocaleSpec extends FlatSpec with Matchers {
  "locales" should "contain three locales if 'de-at' is given" in {
    val data = FakerLocale.DE_AT
    data.locales should equal(Vector("de-at", "de", "en"))
  }

  it should "contain two keys if 'de' is given" in {
    val data = FakerLocale.DE
    data.locales should equal(Vector("de", "en"))
  }

  it should "contain one key if 'en' is given" in {
    val data = FakerLocale.EN
    data.locales should equal(Vector("en"))
  }

}
