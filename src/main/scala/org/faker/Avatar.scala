package org.faker

/**
 * Generates URLs to avatar images
 */
object Avatar extends Base {
  /**
   * returns the url to an avatar image from http://robohash.org
   * @param slug an additional text to be appended to the url. Currently the slug will *not* be escaped.
   * @param locale - the locale to use (defaults to FakerLocale.default)
   */
  def image(slug: String = "")(implicit locale: FakerLocale = FakerLocale.default): String = {
    val s = if (slug.isEmpty) Lorem.words().mkString("+") else slug
    s"http://robohash.org/$s"
  }
}