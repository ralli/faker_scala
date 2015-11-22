package org.faker

import scala.util.Random

/**
 * Generates internet related fake data.
 *
 * {{{
 * scala> Faker.Internet.domainName
 * res1: String = westkihn.info
 *
 * scala> Faker.Internet.userName
 * res2: String = cleora.kreiger
 *
 * scala> Faker.Internet.email
 * res3: String = anissa.feil@vandervort.org
 *
 * scala> Faker.Internet.freeEmail
 * res4: String = terrance_ruecker@yahoo.com
 *
 * scala> Faker.Internet.safeEmail
 * res5: String = cathrine_gottlieb@example.com
 *
 * scala> Faker.Internet.domainWord
 * res7: String = coleabbott
 *
 * scala> Faker.Internet.domainSuffix
 * res8: String = com
 *
 * scala> Faker.Internet.password
 * res9: String = consequaturautemoccaecati
 *
 * scala> Faker.Internet.macAddress()
 * res13: String = 44:d5:ee:d0:f7:fb
 *
 * scala> Faker.Internet.ipV4Address
 * res14: String = 142.246.100.144
 *
 * scala> Faker.Internet.ipV6Address
 * res15: String = 77e2:40cb:3d2a:5697:d6cf:43ae:5cae:e343
 * }}}
 */
object Internet extends Base {
  val SEPARATORS = Array(".", "_")

  /**
   * generates a domain name build of a domain word (a company name without spaces or special chars)
   * and a domain suffix (ex.: com, biz, org, ...)
   */
  def domainName(implicit locale: FakerLocale = FakerLocale.default): String = s"${domainWord.fixUmlauts}.$domainSuffix"

  /**
   * generates a user name build of a persons first and last name delimited by a separator (dot or underscore)
   */
  def userName(implicit locale: FakerLocale = FakerLocale.default): String = {
    // TODO: Make more sophisicated
    val nameParts = Array(Name.firstName, Name.lastName).map { _.replaceAll("""\W""", "") }
    nameParts.mkString(SEPARATORS.randomElement).fixUmlauts.toLowerCase()
  }

  /**
   * generates a persons company email address
   */
  def email(implicit locale: FakerLocale = FakerLocale.default): String = s"${userName()}@${domainName()}"

  /**
   * generates a persons email address at a free email provider (yahoo, gmail and the like)
   */
  def freeEmail(implicit locale: FakerLocale = FakerLocale.default): String = {
    val d = parse("internet.free_email")
    s"$userName@$d"
  }

  /**
   * generates a persons email address at one of the sites example.[com,org,net]
   */

  def safeEmail(implicit locale: FakerLocale = FakerLocale.default): String = {
    val domain = Array("com", "org", "net").randomElement
    s"$userName@example.$domain"
  }

  /**
   * generates the domain word from a generated companies name
   */
  def domainWord(implicit locale: FakerLocale = FakerLocale.default): String = Company.name.split(" ").head.removeNonWordChars.toLowerCase

  /**
   * generates a domain suffix (com, org, net, ...) without leading period
   */
  def domainSuffix(implicit locale: FakerLocale = FakerLocale.default): String = parse("internet.domain_suffix")

  /**
   * generates a password
   */
  def password(implicit locale: FakerLocale = FakerLocale.default): String = Lorem.words().mkString

  /**
   * generates a mac address like `44:d5:ee:d0:f7:fb`
   */
  def macAddress(prefix: String = ""): String = {
    val prefixDigits = prefix.split(":").filterNot(_.isEmpty).map(s => Integer.parseInt(s, 16))
    val addressDigits = (1 to (6 - prefixDigits.size)).map(_ => Random.nextInt(256))
    (prefixDigits ++ addressDigits).map(i => f"$i%02x").mkString(":")
  }

  /**
   * generates an ip address like `142.246.100.144`
   */
  def ipV4Address: String = {
    val ary = 2 to 254
    Array(ary.randomElement, ary.randomElement, ary.randomElement, ary.randomElement).mkString(".")
  }

  /**
   * generates an IP V6 address like `77e2:40cb:3d2a:5697:d6cf:43ae:5cae:e343`
   */
  def ipV6Address: String = {
    val ary = 0 to 65535
    (1 to 8).map(_ => f"${ary.randomElement}%x").mkString(":")
  }
}
