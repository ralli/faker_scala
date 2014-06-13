package org.faker

import java.util.Date
import java.text.SimpleDateFormat

/**
 * Returns faked credit card data
 */
object Business extends Base {
  /**
   * returns a credit card number. the method [[org.faker.Faker.Finance.creditCardNumber( )]] returns better fake data
   * @param locale - the locale to use (defaults to FakerLocale.default)
   */
  def creditCardNumber(implicit locale: FakerLocale = FakerLocale.default): String = parse("business.credit_card_numbers")

  /**
   * returns a credit card expiry date
   * @param locale - the locale to use (defaults to FakerLocale.default)
   */
  def creditCardExpiryDate(implicit locale: FakerLocale = FakerLocale.default): Date = new SimpleDateFormat("yyyy-MM-dd").parse(parse("business.credit_card_expiry_dates"))

  /**
   * returns a credit card type (ex. mastercard)
   */
    def creditCardType(implicit locale: FakerLocale = FakerLocale.default): String = parse("business.credit_card_types")
}
