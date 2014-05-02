package org.faker

import java.util.{Calendar, Date}
import scala.annotation.tailrec

/**
 * Generates credit card data.
 *
 * {{{
 * scala> val creditCardType = Faker.Finance.creditCardType
 * creditCardType: String = visa
 *
 * scala> Faker.Finance.creditCardNumber(creditCardType)
 * res16: String = 4440-3478-9505-8510
 *
 * scala> Faker.Finance.expiryDate
 * res1: java.util.Date = Sun Feb 01 00:00:00 CET 2015
 *
 * scala> Faker.Finance.invalidExpiryDate
 * res2: java.util.Date = Wed May 01 00:00:00 CEST 2013
 * }}}
 *
 */
object Finance extends Base {
  val CREDIT_CARD_TYPES = Array("visa", "mastercard", "discover", "american_express", "diners_club", "jcb", "switch", "solo", "dankort", "maestro", "forbrugsforeningen", "laser")

  /**
   * generates a credit card number.
   *
   * @param typesList the list of credit card types to use (one of [[CREDIT_CARD_TYPES]]
   */
  def creditCardNumber(typesList: String*)(implicit locale: FakerLocale = FakerLocale.default) = {
    val types: Vector[String] = if (typesList.isEmpty) CREDIT_CARD_TYPES.toVector else typesList.toVector
    val t = types.randomElement
    val template = numerify(parse(s"credit_card.$t"))
    val digits = template.filter(_.isDigit)
    val luhnDigit = (10 - (luhn(digits) % 10)) % 10
    template.replaceAll("L", luhnDigit.toString)
  }

  /**
   * Generates a random credit card type to be used by [[creditCardNumber]]
   *
   * See: [[CREDIT_CARD_TYPES]]
   */
  def creditCardType: String = CREDIT_CARD_TYPES.randomElement

  private def firstOfCurrentMonth: Calendar = {
    val c = Calendar.getInstance
    val year = c.get(Calendar.YEAR)
    val month = c.get(Calendar.MONTH)
    c.clear()
    c.set(Calendar.DAY_OF_MONTH, 1)
    c.set(Calendar.MONTH, month)
    c.set(Calendar.YEAR, year)
    c
  }

  /**
   * Generates a valid credit card expiry date. I.e. the expiry date is at least
   * the 1st of the next month.
   *
   * @return the 1st of a month in the future
   */
  def expiryDate: Date = {
    val c = firstOfCurrentMonth
    c.add(Calendar.MONTH, rand(1, 3 * 12))
    c.getTime
  }

  /**
   * Generates an invalid credit card expiry date. I.e. the date is at least the 1st
   * of the previous month.
   *
   * @return the 1st of a month in the past
   */
  def invalidExpiryDate: Date = {
    val c = firstOfCurrentMonth
    c.add(Calendar.MONTH, -rand(1, 3 * 12))
    c.getTime
  }

  private def luhn(s: String): Int = {
    /**
     * Calculates the total sum of the characters using the Luhn algorithm.
     * from scalaz-contrib (StringValidators)
     *
     * https://github.com/typelevel/scalaz-contrib/blob/master/validation-ext/main/scala/validator/StringValidators.scala
     */
    @tailrec
    def luhnSum(str: List[Char], sum: Int, multiplier: Int): Int = {
      def nextMulti(m: Int) = if (m == 1) 2 else 1
      def doubleSum(i: Int) = i % 10 + i / 10
      def digitToInt(x: Char) = x.toInt - '0'.toInt
      str match {
        case Nil => sum
        case x :: xs => luhnSum(xs, sum + doubleSum(digitToInt(x) * multiplier), nextMulti(multiplier))
      }
    }
    luhnSum(s.toList, 0, 2)
  }
}
