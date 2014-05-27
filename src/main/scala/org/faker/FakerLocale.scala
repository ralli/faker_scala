package org.faker

/**
 * Provides Information about which locale to use for fake data generation.
 *
 * The faker objects will use a sequence of data files representing different locales to generate their values.
 * The "de-at" locale, for instance, will use the data files de_at.yml, de.yml and en.yml (in that sequence). The companion
 * object will provide constants for all locales supported by this faker library.
 *
 * {{{
 * scala> Faker.Name.name // will use the default locale (FakerLocale.EN)
 * res5: String = Cleve Christiansen
 *
 * scala> Faker.Name.name(Faker.FakerLocale.DE) // uses the DE-locale and EN as fallback
 * res7: String = Lina von Harnapp
 *
 * scala> implicit val defaultLocale = Faker.FakerLocale.DE_AT // will use the "DE-AT" as locale and "DE" and "EN" as fallback
 * defaultLocale: org.faker.Faker.FakerLocale = FakerLocale(de-at)
 *
 * scala> Faker.Name.name
 * res12: String = Cosima Oschkenat
 *
 * }}}
 * @param localeKey
 */
case class FakerLocale(localeKey: String) {
  val fallbackLanguage = "en"
  lazy val locales = (localesList(localeKey) :+ fallbackLanguage).distinct

  def localesList(locale: String) = {
    val pattern = "^(\\w+)-.*$".r
    pattern.findFirstIn(locale) match {
      case Some(pattern(firstPart)) =>
        if(FakerLocale.allLocales.exists(fl => fl.localeKey == firstPart))
          Vector(locale, firstPart)
        else
          Vector(locale)
      case _ =>
        Vector(locale)
    }
  }
}

/**
 * companion object for [[FakerLocale]]
 *
 * provides all supported locales supported.
 *
 * {{{
 * scala> Faker.FakerLocale.EN
 * res13: org.faker.Faker.FakerLocale = FakerLocale(en)
 *
 * Faker.FakerLocale.allLocales
 * res14: scala.collection.immutable.Vector[org.faker.Faker.FakerLocale] = Vector(FakerLocale(de-at), ...)
 * }}}
 */
object FakerLocale {
  val DE_AT = FakerLocale("de-at")
  val DE_CH = FakerLocale("de-ch")
  val DE = FakerLocale("de")
  val EN_AU = FakerLocale("en-au")
  val EN_BORK = FakerLocale("en-bork")
  val EN_CA = FakerLocale("en-ca")
  val EN_GB = FakerLocale("en-gb")
  val EN_IND = FakerLocale("en-ind")
  val EN_NEP = FakerLocale("en-nep")
  val EN_US = FakerLocale("en-us")
  val EN = FakerLocale("en")
  val ES = FakerLocale("es")
  val FA = FakerLocale("fa")
  val FR = FakerLocale("fr")
  val IT = FakerLocale("it")
  val JA = FakerLocale("ja")
  val KO = FakerLocale("ko")
  val NB_NO = FakerLocale("nb-no")
  val NL = FakerLocale("nl")
  val PL = FakerLocale("pl")
  val PT_BR = FakerLocale("pt-br")
  val RU = FakerLocale("ru")
  val SK = FakerLocale("sk")
  val VI = FakerLocale("vi")

  val default = FakerLocale.EN

  val allLocales = Vector(DE_AT, DE_CH, DE, EN_AU, EN_BORK, EN_CA, EN_GB, EN_IND, EN_NEP, EN_US, EN, ES, FA, FR, IT, JA, KO, NB_NO, NL, PL, PT_BR, RU, SK, VI)
}
