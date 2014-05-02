package org.faker

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
  def phoneNumber(implicit locale: FakerLocale = FakerLocale.default) = numerify(parse("phone_number.formats"))

  /**
   * generates a cell phone number. Depending on the locale the cell phone number might not be defined. In this
   * case the method will fall back to ordinary phone numbers.
   */
  def cellPhone(implicit locale: FakerLocale = FakerLocale.default) = parseSafe("cell_phone.formats").map(s => numerify(s)).getOrElse(phoneNumber)
}
