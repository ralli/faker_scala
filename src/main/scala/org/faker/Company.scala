package org.faker

/**
 * Generates company related data.
 *
 * {{{
 * scala> Faker.Company.name
 * res11: String = Dickinson Group
 *
 * scala> Faker.Company.catchPhrase
 * res17: String = Innovative 5th generation implementation
 *
 * scala> Faker.Company.bullshit
 * res0: String = revolutionize cross-platform experiences
 *
 * scala> Faker.Company.dunsNumber
 * res1: String = 95-709-4393
 * }}}
 */
object Company extends Base {
  def name(implicit locale: FakerLocale = FakerLocale.default) = parse("company.name")

  def catchPhrase(implicit locale: FakerLocale = FakerLocale.default) = {
    val seqs = getSeq("company.buzzwords").get
    seqs.map(s => s.randomElement).mkString(" ")
  }

  def bullshit(implicit locale: FakerLocale = FakerLocale.default) = {
    val seqs = getSeq("company.bs").get
    seqs.map(s => s.randomElement).mkString(" ")
  }

  /**
   * generates a companies D-U-N-S number.
   */
  def dunsNumber(implicit locale: FakerLocale = FakerLocale.default) = numerify("##-###-####")
}
