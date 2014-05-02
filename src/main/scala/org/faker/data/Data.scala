package org.faker.data

import org.faker.FakerLocale

/**
 * Provides access to the fake data definitions in the various locale dependent files.
 *
 * {{{
 * scala> val data = new Faker.Data
 * data: org.faker.Faker.Data = org.faker.Faker$Data@dbedbf6
 *
 * scala> data.get("name.name")
 * res3: Option[Seq[String]] = Some(Buffer(#{prefix} #{first_name} #{last_name}, ...)
 *
 * scala> data.getSeq("company.buzzwords")
 * res4: Option[Seq[Seq[String]]] = Some(ArrayBuffer(Buffer(Adaptive, Advanced, Ameliorated, ...))
 * }}}
 */
class Data {
  val allLocales = FakerLocale.allLocales.map(_.localeKey)

  val localeDataMap = initLocaleDataMap


  private def initLocaleDataMap = allLocales.map {
    locale =>
      locale -> LocaleData(locale)
  }.toMap


  private def localeDataKey(locale: String, key: String) = s"$locale.faker.$key"

  /**
   * Given the data´s key returns a list of alternatives for fake data.
   * See [[LocaleData.get( )]] for details.
   *
   * @param key the datas key without the prefix `<locale>`.`faker`
   * @param locale - the locale to use (defaults to FakerLocale.default)
   * @return The sequence of alternatives or `None`
   */
  def get(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[Seq[String]] = {
    /*
     * the stream is evaluated lazily. So if a match is found in the 'de' locale,
     * the 'en' locale search will not be executed
     */
    val stream: Stream[Seq[String]] = for {
      locale <- locale.locales.toStream
      localeData <- localeDataMap.get(locale)
      data <- localeData.get(localeDataKey(locale, key))
    } yield data
    stream.headOption
  }

  /**
   * given a key returns a sequence of sequences to generate fake data.
   *
   * an example is [[org.faker.Company.catchPhrase]] which uses the key 'company.buzzwords' to generate
   * phases like "Front-line grid-enabled protocol" from three sequences (one for each word).
   *
   * @param key the datas key without the prefix `<locale>`.`faker`
   * @param locale - the locale to use (defaults to FakerLocale.default)
   * @return the sequence of sequences or `None`
   */
  def getSeq(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[Seq[Seq[String]]] = {
    /*
     * the stream is evaluated lazily. So if a match is found in the 'de' locale,
     * the 'en' locale search will not be executed
     */
    val stream: Stream[Seq[Seq[String]]] = for {
      locale <- locale.locales.toStream
      localeData <- localeDataMap.get(locale)
      data <- localeData.getSeq(localeDataKey(locale, key))
    } yield data
    stream.headOption
  }
}

object Data {
  val data = new Data()

  def get(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[Seq[String]] = data.get(key)

  def getSeq(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[Seq[Seq[String]]] = data.getSeq(key)
}