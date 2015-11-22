package org.faker

import scala.util.Random

/**
 * Computes fake addresses.

 * Examples:
 *
 * {{{
 * scala> Faker.Address.city
 * res0: String = Port Zora
 *
 * scala> Faker.Address.streetName
 * res3: String = Corkery Street
 *
 * scala> Faker.Address.timeZone
 * res38: String = Europe/Sofia
 *
 * scala> Faker.Address.streetAddress()
 * res42: String = 16334 Eulalia Throughway
 * }}}
 */
object Address extends Base {
  /**
   * Returns a cities name
   *
   * @param locale - the locale to use (defaults to FakerLocale.default)
   */
  def city(implicit locale: FakerLocale = FakerLocale.default): String = parse("address.city")

  /**
   * Returns a street name without any additions like building number or secondary address
   *
   * @param locale - the locale to use (defaults to FakerLocale.default)
   */
  def streetName(implicit locale: FakerLocale = FakerLocale.default): String = parse("address.street_name")

  /**
   * Returns a street name including an (optional) building number
   *
   * @param includeSecondary `true` if the street address should include addtitional lnformation like 'Apt. 201' or 'Suite 21'
   * @param locale - the locale to use (defaults to FakerLocale.default)
   */
  def streetAddress(includeSecondary: Boolean = false)(implicit locale: FakerLocale = FakerLocale.default): String = {
    val base = parse("address.street_address")
    val result = if (includeSecondary) base + " " + secondaryAddress else base
    numerify(result)
  }

  /**
   * Returns additional information to a streets address like 'Apt. 201' or 'Suite 21'
   *
   * @param locale - the locale to use (defaults to FakerLocale.default)
   */
  def secondaryAddress(implicit locale: FakerLocale = FakerLocale.default): String = numerify(parse("address.secondary_address"))

  /**
   * @param locale - the locale to use (defaults to FakerLocale.default)
   * @return the building number
   */
  def buildingNumber(implicit locale: FakerLocale = FakerLocale.default): String = bothify(parse("address.building_number"))

  /**
   * @param locale - the locale to use (defaults to FakerLocale.default)
   *
   * @return the zip code
   */
  def zipCode(implicit locale: FakerLocale = FakerLocale.default): String = bothify(parse("address.postcode"))

  /**
   * @param locale - the locale to use (defaults to FakerLocale.default)
   * @return Time Zone in a long ruby-like format (ex. America/Denver)
   */
  def timeZone(implicit locale: FakerLocale = FakerLocale.default): String = bothify(parse("address.time_zone"))

  /**
   * an alias for [[zipCode]]
   */
  def zip(implicit locale: FakerLocale = FakerLocale.default): String = zipCode

  /**
   * an alias for [[zipCode]]
   */
  def postcode(implicit locale: FakerLocale = FakerLocale.default): String = zipCode

  /**
   * returns an abbreviated state name (ex. AK for Alaska)
   *
   * @param locale - the locale to use (defaults to FakerLocale.default)
   */
  def stateAbbr(implicit locale: FakerLocale = FakerLocale.default): String = parse("address.state_abbr")

  /**
   * returns a states name (ex. Alaska)
   * @param locale - the locale to use (defaults to FakerLocale.default)
   */
  def state(implicit locale: FakerLocale = FakerLocale.default): String = parse("address.state")

  /**
   * Returns a countries name (ex. Albania)
   * @param locale - the locale to use (defaults to FakerLocale.default)
   */
  def country(implicit locale: FakerLocale = FakerLocale.default): String = parse("address.country")

  def latitude: String = (Random.nextDouble * 180 - 90).toString

  def longitude: String = (Random.nextDouble * 180 - 90).toString
}
