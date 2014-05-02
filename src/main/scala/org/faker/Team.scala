package org.faker

/**
 * Generates Names of fake football [baseball, hockey, ...] teams.
 *
 * {{{
 * scala> Faker.Team.name
 * res38: String = Nevada Warlocks
 * }}}
 */
object Team extends Base {
  def name(implicit locale: FakerLocale = FakerLocale.default) = parse("team.name").titlelize
}
