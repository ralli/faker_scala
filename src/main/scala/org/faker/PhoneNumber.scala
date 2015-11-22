package org.faker

import java.util.NoSuchElementException

/**
 * Generates phone numbers
 * {{{
 * scala> Faker.PhoneNumber.phoneNumber
 * res32: String = (683)319-1686
 *
 * scala> Faker.PhoneNumber.cellPhone(Faker.FakerLocale.DE)
 * res34: String = +49-1656-50459299
 * }}}
 */
object PhoneNumber extends Base {
  /**
   * Generates a random phone number in various formats.
   */
  def phoneNumber(implicit locale: FakerLocale = FakerLocale.default): String = numerify(parse("phone_number.formats"))

  /**
   * generates a cell phone number. Depending on the locale the cell phone number might not be defined. In this
   * case the method will fall back to ordinary phone numbers.
   */
  def cellPhone(implicit locale: FakerLocale = FakerLocale.default): String = parseSafe("cell_phone.formats").map(s => numerify(s)).getOrElse(phoneNumber)

  /**
   * generates the phone numbers area code (US only).
   */
  @throws[NoSuchElementException]("if the locale is not EN_US")
  def areaCode(implicit locale: FakerLocale = FakerLocale.default): String = parse("phone_number.area_code")

  /**
   * generates the phone numbers exchange code (US only)
   */
  @throws[NoSuchElementException]("if the locale is not EN_US")
  def exchangeCode(implicit locale: FakerLocale = FakerLocale.default): String = numerify(parse("phone_number.exchange_code"))
}
