package org.faker

import scala.util.Random

/**
 * Generates properties for Products (sold in an online shop for example).
 *
 * {{{
 * scala> Faker.Commerce.color
 * res1: String = pink
 *
 * scala> Faker.Commerce.department
 * res2: String = Grocery, Health & Beauty
 *
 * scala> Faker.Commerce.productName
 * res8: String = Awesome Rubber Car
 *
 * scala> Faker.Commerce.price
 * res9: Double = 71.33
 * }}}
 */
object Commerce extends Base {
  def color(implicit locale: FakerLocale = FakerLocale.default): String = parse("commerce.color")

  def department(implicit locale: FakerLocale = FakerLocale.default): String = parse("commerce.department")

  def productName(implicit locale: FakerLocale = FakerLocale.default): String = parse("commerce.product_name.adjective") + " " + parse("commerce.product_name.material") + " " + parse("commerce.product_name.product")

  def price(implicit locale: FakerLocale = FakerLocale.default): Double = (Random.nextDouble * 100 * 100).floor / 100.0
}
