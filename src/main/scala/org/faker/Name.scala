package org.faker

/**
 * Generates fake data related to persons names.
 *
 * {{{
 * scala> Faker.Name.name
 * res20: String = Dr. Ceasar Moore
 *
 * scala> Faker.Name.firstName
 * res22: String = Alessandra
 *
 * scala> Faker.Name.lastName
 * res23: String = Weber
 *
 * scala> Faker.Name.prefix
 * res24: String = Miss
 *
 * scala> Faker.Name.suffix
 * res25: String = II
 * }}}
 */
object Name extends Base {
  /**
   * Generates a persons full name built from the persons first and last name and optionally some additional parts
   * like the persons title.
   */
  def name(implicit locale: FakerLocale = FakerLocale.default): String = parse("name.name")

  def firstName(implicit locale: FakerLocale = FakerLocale.default): String = parse("name.first_name")

  def lastName(implicit locale: FakerLocale = FakerLocale.default): String = parse("name.last_name")

  /**
   * Generates a names prefix like 'Mr.' or an academic title (depending on the locale)
   */
  def prefix(implicit locale: FakerLocale = FakerLocale.default): String = parse("name.prefix")

  /**
   * Generates a names suffix like 'MD' or 'III' (depending on the locale)
   */
  def suffix(implicit locale: FakerLocale = FakerLocale.default): String = parse("name.suffix")

  /**
   * Generate a buzzword-laden job title
   *
   * Wordlist from http://www.bullshitjob.com/title/
   */
  def jobTitle(implicit locale: FakerLocale = FakerLocale.default): String = parse("name.title.descriptor") + " " + parse("name.title.level") + " " + parse("name.title.job")
}
