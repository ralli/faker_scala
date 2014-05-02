package org.faker

import scala.util.Random


/**
 * Generates number related fake data.
 *
 * {{{
 * scala> Faker.Number.number(10)
 * res26: String = 4610283377
 *
 * scala> Faker.Number.decimal(3,2)
 * res28: String = 131.06
 *
 * scala> Faker.Number.digit
 * res30: String = 2
 * }}}
 */
object Number extends Base {
  /**
   * Generates a string of random digits without leading zeros
   *
   * @param digits the number of digits generate
   */
  def number(digits: Int) = numerify("#" * digits)

  /**
   * Generates a String of digits. the digits will be delimited by a period independent of the locale used.
   * @param ldigits the number of digits to generate before the period
   * @param rdigits the number of digits to generate after the period
   */
  def decimal(ldigits: Int, rdigits: Int = 2) = numerify(("#" * ldigits) + '.' + ("#" * rdigits))

  /**
   * Generates a single digit string
   */
  def digit = Random.nextInt(10).toString
}
