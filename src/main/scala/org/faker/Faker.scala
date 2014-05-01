package org.faker

import util.Random
import org.yaml.snakeyaml.Yaml
import scala.collection.JavaConverters._
import scala.annotation.tailrec
import java.util.regex.Pattern
import scala.util.matching.Regex.Match
import java.security.MessageDigest
import java.util.{Calendar, NoSuchElementException, Date}
import java.text.SimpleDateFormat
import scala.NoSuchElementException

object Faker {

  /**
   * Provides Information about which locale to use for fake data generation.
   *
   * The faker objects will use a sequence of data files representing different locales to generate their values.
   * The "de-at" locale, for instance, will use the data files de_at.yml, de.yml and en.yml (in that sequence). The companion
   * object will provide constants for all locales supported by this faker library.
   *
   * {{{
   * scala> Faker.Name.name // will use the default locale (FakerLocale.EN)
   * res5: String = Cleve Christiansen
   *
   * scala> Faker.Name.name(Faker.FakerLocale.DE) // uses the DE-locale and EN as fallback
   * res7: String = Lina von Harnapp
   *
   * scala> implicit val defaultLocale = Faker.FakerLocale.DE_AT // will use the "DE-AT" as locale and "DE" and "EN" as fallback
   * defaultLocale: org.faker.Faker.FakerLocale = FakerLocale(de-at)
   *
   * scala> Faker.Name.name
   * res12: String = Cosima Oschkenat
   *
   * }}}
   * @param localeKey
   */
  case class FakerLocale(localeKey: String) {
    val fallbackLanguage = "en"
    lazy val locales = (localesList(localeKey) :+ fallbackLanguage).distinct

    def localesList(locale: String) = {
      val pattern = "^(\\w+)-.*$".r
      pattern.findFirstIn(locale) match {
        case Some(pattern(firstPart)) =>
          Vector(locale, firstPart)
        case _ =>
          Vector(locale)
      }
    }
  }

  /**
   * companion object for [[FakerLocale]]
   *
   * provides all supported locales supported.
   *
   * {{{
   * scala> Faker.FakerLocale.EN
   * res13: org.faker.Faker.FakerLocale = FakerLocale(en)
   *
   * Faker.FakerLocale.allLocales
   * res14: scala.collection.immutable.Vector[org.faker.Faker.FakerLocale] = Vector(FakerLocale(de-at), ...)
   * }}}
   */
  object FakerLocale {
    val DE_AT = FakerLocale("de-at")
    val DE_CH = FakerLocale("de-ch")
    val DE = FakerLocale("de")
    val EN_AU = FakerLocale("en-au")
    val EN_BORK = FakerLocale("en-bork")
    val EN_CA = FakerLocale("en-ca")
    val EN_GB = FakerLocale("en-gb")
    val EN_IND = FakerLocale("en-ind")
    val EN_NEP = FakerLocale("en-nep")
    val EN_US = FakerLocale("en-us")
    val EN = FakerLocale("en")
    val ES = FakerLocale("es")
    val FA = FakerLocale("fa")
    val FR = FakerLocale("fr")
    val IT = FakerLocale("it")
    val JA = FakerLocale("ja")
    val KO = FakerLocale("ko")
    val NB_NO = FakerLocale("nb-no")
    val NL = FakerLocale("nl")
    val PL = FakerLocale("pl")
    val PT_BR = FakerLocale("pt-br")
    val RU = FakerLocale("ru")
    val SK = FakerLocale("sk")
    val VI = FakerLocale("vi")

    val default = FakerLocale.EN

    val allLocales = Vector(DE_AT, DE_CH, DE, EN_AU, EN_BORK, EN_CA, EN_GB, EN_IND, EN_NEP, EN_US, EN, ES, FA, FR, IT, JA, KO, NB_NO, NL, PL, PT_BR, RU, SK, VI)
  }

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
  private[faker] class Data {
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
     * an example is [[Company.catchPhrase]] which uses the key 'company.buzzwords' to generate
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

  private[faker] case class LocaleData(locale: String) {
    lazy val yamlData = load(filePathOf(locale))

    private def filePathOf(locale: String) = {
      s"/$locale.yml"
    }

    private def load(name: String) = {
      val yaml = new Yaml()
      yaml.load(classOf[LocaleData].getResourceAsStream(name)).asInstanceOf[java.util.Map[String, Object]]
    }

    /**
     * Given the data´s key returns a list of alternatives for fake data.
     *
     * The key is built upon a sequence of sub keys delimited by periods.
     * For a complete reference of possible keys one should refer to the various YAML files (ex. en.yml).
     *
     * Given a yaml file
     *
     * {{{
     *  en:
     *    faker:
     *      dummy:
     *        testkeys: [One, Two, Three]
     * }}}
     *
     * the statement
     *
     * {{{
     *  data.get("dummy.testkeys")
     * }}}
     *
     * will return
     *
     * {{{
     * Some(Buffer(One, Two, Three)
     * }}}
     *
     *
     * @param key the datas key without the prefix `<locale>`.`faker`
     * @return The sequence of alternatives or `None`
     */

    def get(key: String): Option[Seq[String]] = {
      val keys = key.split("\\.").toList
      get(keys, Some(yamlData))
    }

    @tailrec
    private def get(keys: List[String], data: Option[Object]): Option[Seq[String]] = {
      data match {
        case None => None
        case Some(map: java.util.Map[_, _]) if !keys.isEmpty =>
          get(keys.tail, Option(map.asInstanceOf[java.util.Map[String, Object]].get(keys.head)))
        case Some(list: java.util.List[_]) if keys.isEmpty =>
          Some(list.asInstanceOf[java.util.List[String]].asScala)
        case Some(s: String) =>
          Some(Vector(s))
        case _ => None
      }
    }

    /**
     * given a key returns a sequence of sequences to generate fake data.
     *
     * an example is [[Company.catchPhrase]] which uses the key 'company.buzzwords' to generate
     * phases like "Front-line grid-enabled protocol" from three sequences (one for each word).
     *
     * @param key the datas key without the prefix `<locale>`.`faker`
     * @return the sequence of sequences or `None`
     */

    def getSeq(key: String): Option[Seq[Seq[String]]] = {
      val keys = key.split("\\.").toList
      getSeq(keys, Some(yamlData))
    }

    @tailrec
    private def getSeq(keys: List[String], data: Option[Object]): Option[Seq[Seq[String]]] = {
      data match {
        case None => None
        case Some(map: java.util.Map[_, _]) if !keys.isEmpty =>
          getSeq(keys.tail, Option(map.asInstanceOf[java.util.Map[String, Object]].get(keys.head)))
        case Some(list: java.util.List[_]) if keys.isEmpty =>
          val lists = list.asInstanceOf[java.util.List[java.util.List[String]]]
          Some(lists.asScala.map(l => l.asScala))
        case _ => None
      }
    }
  }

  private[faker] object DataHolder {
    val data = new Data()

    def get(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[Seq[String]] = data.get(key)

    def getSeq(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[Seq[Seq[String]]] = data.getSeq(key)
  }

  trait Base {

    implicit class StringExtensions(string: String) {
      def gsub(re: String, replacement: String): String = {
        re.r.replaceAllIn(string, replacement)
      }

      def gsub(re: String)(m: Match => String): String = {
        re.r.replaceAllIn(string, m)
      }

      def fixUmlauts = {
        """[äöüßÄÖÜ]""".r.replaceAllIn(string, m => m.toString() match {
          case "ä" => "ae"
          case "ö" => "oe"
          case "ü" => "ue"
          case "ß" => "ss"
          case "Ä" => "Ae"
          case "Ö" => "Oe"
          case "Ü" => "Ue"
        })
      }

      def toSnakeCase =
        gsub("^[A-Z]")(m => m.toString.toLowerCase()).
          gsub( """([A-Z])""")(m => "_" + m.group(1).toLowerCase())

      def titlelize = string.gsub("(\\s+.)")(m => m.group(1).toUpperCase())

      def removeNonWordChars = string.replaceAll("""\W""", "")
    }

    implicit class RandomInArray[T](coll: Array[T]) {
      def randomElement: T = coll(Random.nextInt(coll.size))
    }

    implicit class RandomInSeq[T](coll: Seq[T]) {
      def randomElement: T = coll(Random.nextInt(coll.size))

      def sample(size: Int): Seq[T] = (1 to size).map(i => coll(Random.nextInt(coll.size)))
    }

    private val letters = ('a' to 'z') ++ ('A' to 'Z')
    private val digits = '0' to '9'

    /**
     * Replaces all occurrences of hash signs '#' with random digits.
     *
     * Ensures the first ditit is not zero.
     *
     * Example: `numerify("ABC##-!##")` may return `"ABC10-!83"`.
     *
     * @return a string with all hash signs replaced
     */
    def numerify(s: String): String = {
      // make sure the first digit is not zero
      val s1 = "#".r.replaceFirstIn(s, (Random.nextInt(9) + 1).toString)
      s1.map(c => if (c == '#') digits.randomElement else c)
    }

    /**
     * Replaces all question marks '?' with random characters (uppercase and lowercase out of 'A' to 'Z').
     *
     * @return a string with all question marks replaced
     */
    def letterify(s: String): String = s.map(c => if (c == '?') letters.randomElement else c)

    def bothify(s: String): String = letterify(numerify(s))

    private[faker] def rand(from: Int, to: Int): Int = from + Random.nextInt(to - from + 1)

    /**
     * Straight translation of the original Ruby Implementation (i have to admit the scala version became even uglier):
     *
     * Given a regular expression, attempt to generate a string
     * that would match it.  This is a rather simple implementation,
     * so don't be shocked if it blows up on you in a spectacular fashion.
     *
     * It does not handle `.`, `*`, unbounded ranges such as `{1,}`,
     * extensions such as `(?=)`, character classes, some abbreviations
     * for character classes, and nested parentheses.
     *
     * I told you it was simple. :) It's also probably dog-slow,
     * so you shouldn't use it.
     *
     * It will take a regex like this:
     *
     * {{{/^[A-PR-UWYZ0-9][A-HK-Y0-9][AEHMNPRTVXY0-9]?[ABEHMNPRVWXY0-9]? {1,2}[0-9][ABD-HJLN-UW-Z]{2}$/}}}
     *
     * and generate a string like this:
     *
     * {{{"U3V  3TP"}}}
     */
    def regexify(re: String) = {
      re.
        gsub( """^\/?\^?""", "").gsub( """\$?\/?$""", ""). // Ditch the anchors
        gsub( """\{(\d+)\}""", """\{$1,$1\}""").gsub( """\?""", "{0,1}"). // All {2} become {2,2} and ? become {0,1}
        gsub( """(\[[^\]]+\])\{(\d+),(\d+)\}""")(m => m.group(1) * rand(m.group(2).toInt, m.group(3).toInt)). // [12]{1,2} becomes [12] or [12][12]
        gsub( """(\([^\)]+\))\{(\d+),(\d+)\}""")(m => m.group(1) * rand(m.group(2).toInt, m.group(3).toInt)). // (12|34){1,2} becomes (12|34) or (12|34)(12|34)
        // TODO \d{1,3} does not work here and returns ddd instead of \d\d\d
        gsub( """(\\?.)\{(\d+),(\d+)\}""")(m => m.group(1) * rand(m.group(2).toInt, m.group(3).toInt)). // A{1,2} becomes A or AA
        gsub( """\((.*?)\)""")(m => m.toString().gsub( """[\(\)]""", "").split('|').randomElement). // (this|that) becomes 'this' or 'that'
        gsub( """\[([^\]]+)\]""")(m => m.toString().gsub( """(\w)-(\w)""")(range => (range.group(1)(0) to range.group(2)(0)).randomElement.toString)). // All A-Z inside of [] become C (or X, or whatever)
        gsub( """\[([^\]]+)\]""")(m => m.group(1).toArray.randomElement.toString). // All [ABC] become B (or A or C)
        gsub( """\\d""")(m => ('0' to '9').randomElement.toString).
        gsub( """\\w""")(m => letters.randomElement.toString)
    }

    /**
     * given a string with Ruby placeholders returns a list of the placeholders.
     *
     * Example: `extractSubKeys("#{first_name} #{last_name}")` will return `["first_name", "last_name"]`
     */
    private def extractSubKeys(key: String): List[String] = {
      val pattern = "#\\{([^}]+)\\}".r
      pattern.findAllMatchIn(key).map(m => m.group(1)).toList
    }

    /**
     * looks up something like `first_name` or `Name.first_name` in the YAML file
     */
    private def expandSubKey(baseKey: String, subKey: String)(implicit locale: FakerLocale = FakerLocale.default): String = {
      if (subKey.contains('.')) {
        parseSafe(subKey.toSnakeCase).getOrElse(s"#{$subKey}")
      }
      else {
        val basePrefix = baseKey.split("\\.").init.mkString(".")
        parseSafe(s"$basePrefix.$subKey").getOrElse(s"#{$subKey}")
      }
    }

    /**
     * expands a string with ruby placeholders.
     *
     * Example: `expandValues("name.name", "#{first_name} #{last_name}")` will become something like `"Abraham Lincoln"`.
     *
     * The translation rules are as follows:
     *
     * 1. Everything that looks like a regular expression (starts and ends with a slash) will be processed by the [[regexify( )]] method.
     * 2. Ruby placeholders (ex. `#{Name.first_name}` `#{first_name}`) are expanded by looking up (and expanding) the appropriate key in the YAML file.
     *
     * @param key the key of the value to be expanded
     * @param value the value to be expanded
     */
    def expandValues(key: String, value: String)(implicit locale: FakerLocale = FakerLocale.default): String = {
      def looksLikeRegexp(s: String) = value.matches( """^/.*/$""")
      if (looksLikeRegexp(value)) {
        regexify(value)
      }
      else {
        val subKeys = extractSubKeys(value)
        val expandedKeys: Vector[(String, String)] = subKeys.foldLeft(Vector.empty[(String, String)]) {
          (map, subKey) => map :+ (subKey -> expandSubKey(key, subKey))
        }
        expandedKeys.foldLeft(value) {
          case (res, (k, v)) =>
            res.replaceFirst(Pattern.quote(s"#{$k}"), v)
        }
      }
    }

    /**
     * gets a single random value by its key.
     *
     * The value will not be processed by [[expandValues( )]] and may contain ruby placeholders or regular expressions.
     */
    def fetch(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[String] = get(key).map(seq => seq.randomElement)

    /**
     * gets a single random value by its key and expands the value using [[expandValues( )]].
     */
    def parseSafe(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[String] = fetch(key).map(value => expandValues(key, value))

    /**
     * gets a single random value by its key and expands the value using [[expandValues( )]].
     */
    @throws[NoSuchElementException]("if the key does not exist")
    def parse(key: String)(implicit locale: FakerLocale = FakerLocale.default): String = parseSafe(key).getOrElse(throw new NoSuchElementException(key))

    /**
     * Given the data´s key returns a list of alternatives for fake data.
     * See [[LocaleData.get( )]] for details.
     */
    def get(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[Seq[String]] = DataHolder.get(key)

    /**
     * given a key returns a sequence of sequences to generate fake data.
     * See [[LocaleData.getSeq( )]] for details.
     */
    def getSeq(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[Seq[Seq[String]]] = DataHolder.getSeq(key)
  }

  /**
   * Computes fake addresses.

   * Examples:
   *
   * {{{
   * scala> Faker.Address.city
   * res0: String = Port Zora
   *
   * scala> Faker.Address.streetName
   * res3: String = Corkery Street
   *
   * scala> Faker.Address.timeZone
   * res38: String = Europe/Sofia
   *
   * scala> Faker.Address.streetAddress()
   * res42: String = 16334 Eulalia Throughway
   * }}}
   */
  object Address extends Base {
    /**
     * Returns a cities name
     *
     * @param locale - the locale to use (defaults to FakerLocale.default)
     */
    def city(implicit locale: FakerLocale = FakerLocale.default) = parse("address.city")

    /**
     * Returns a street name without any additions like building number or secondary address
     *
     * @param locale - the locale to use (defaults to FakerLocale.default)
     */
    def streetName(implicit locale: FakerLocale = FakerLocale.default) = parse("address.street_name")

    /**
     * Returns a street name including an (optional) building number
     *
     * @param includeSecondary `true` if the street address should include addtitional lnformation like 'Apt. 201' or 'Suite 21'
     * @param locale - the locale to use (defaults to FakerLocale.default)
     */
    def streetAddress(includeSecondary: Boolean = false)(implicit locale: FakerLocale = FakerLocale.default) = {
      val base = parse("address.street_address")
      val result = if (includeSecondary) base + secondaryAddress else base
      numerify(result)
    }

    /**
     * Returns additional information to a streets address like 'Apt. 201' or 'Suite 21'
     *
     * @param locale - the locale to use (defaults to FakerLocale.default)
     */
    def secondaryAddress(implicit locale: FakerLocale = FakerLocale.default) = numerify(parse("address.secondary_address"))

    /**
     * @param locale - the locale to use (defaults to FakerLocale.default)
     * @return the building number
     */
    def buildingNumber(implicit locale: FakerLocale = FakerLocale.default) = bothify(parse("address.building_number"))

    /**
     * @param locale - the locale to use (defaults to FakerLocale.default)
     *
     * @return the zip code
     */
    def zipCode(implicit locale: FakerLocale = FakerLocale.default) = bothify(parse("address.postcode"))

    /**
     * @param locale - the locale to use (defaults to FakerLocale.default)
     * @return Time Zone in a long ruby-like format (ex. America/Denver)
     */
    def timeZone(implicit locale: FakerLocale = FakerLocale.default) = bothify(parse("address.time_zone"))

    /**
     * an alias for [[zipCode]]
     */
    def zip(implicit locale: FakerLocale = FakerLocale.default) = zipCode

    /**
     * an alias for [[zipCode]]
     */
    def postcode(implicit locale: FakerLocale = FakerLocale.default) = zipCode

    /**
     * returns an abbreviated state name (ex. AK for Alaska)
     *
     * @param locale - the locale to use (defaults to FakerLocale.default)
     */
    def stateAbbr(implicit locale: FakerLocale = FakerLocale.default) = parse("address.state_abbr")

    /**
     * returns a states name (ex. Alaska)
     * @param locale - the locale to use (defaults to FakerLocale.default)
     */
    def state(implicit locale: FakerLocale = FakerLocale.default) = parse("address.state")

    /**
     * Returns a countries name (ex. Albania)
     * @param locale - the locale to use (defaults to FakerLocale.default)
     */
    def country(implicit locale: FakerLocale = FakerLocale.default) = parse("address.country")

    def latitude = (Random.nextDouble * 180 - 90).toString

    def longitude = (Random.nextDouble * 180 - 90).toString
  }

  /**
   * Generates URLs to avatar images
   */
  object Avatar extends Base {
    /**
     * returns the url to an avatar image from http://robohash.org
     * @param slug an additional text to be appended to the url. Currently the slug will *not* be escaped.
     * @param locale - the locale to use (defaults to FakerLocale.default)
     */
    def image(slug: String = "")(implicit locale: FakerLocale = FakerLocale.default) = {
      val s = if (slug.isEmpty) Lorem.words().mkString("+") else slug
      s"http://robohash.org/$s"
    }
  }

  /**
   * generates bitcoin addresses
   *
   * A bitcoin address is in fact the hash of a ECDSA public key, computed this way:
   *
   * Version = 1 byte of 0 (zero); on the test network, this is 1 byte of 111 (we will always use '0' zero)
   *
   * Key hash = Version concatenated with RIPEMD-160(SHA-256(public key))
   *
   * Checksum = 1st 4 bytes of SHA-256(SHA-256(Key hash))
   *
   * Bitcoin Address = Base58Encode(Key hash concatenated with Checksum)
   *
   */
  object Bitcoin extends Base {
    val alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".toArray
    val base = alphabet.length
    val mac = MessageDigest.getInstance("SHA-256")

    /**
     * returns a bitcoin address
     */
    def address: String = {
      val byteStream: Stream[Byte] = Stream.continually(Random.nextInt().toByte)
      val byteArray: Array[Byte] = (0.toByte #:: byteStream.take(20)).toArray
      // due to https://en.bitcoin.it/wiki/Protocol_specification#Hashes the checksum is four bytes long
      // (not three as in the ruby implementaion)
      val checkSum = sha2(byteArray).take(4)
      base58(byteArray ++ checkSum)
    }

    private def base58(a: Array[Byte]): String = {
      @tailrec
      def dec(n: BigInt, acc: List[Char]): String = {
        if (n == 0)
          acc.mkString
        else {
          val div = n / base
          val mod = n % base
          val c = alphabet(mod.intValue())
          dec(div, c :: acc)
        }
      }

      dec(BigInt(1, a), Nil)
    }

    private def sha2(a: Array[Byte]): Array[Byte] = mac.digest(a)
  }

  /**
   * Returns faked credit card data
   */
  object Business extends Base {
    /**
     * returns a credit card number. the method [[Finance.creditCardNumber( )]] returns better fake data
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

  /**
   * generates ISBN numbers
   */
  object Code extends Base {
    /**
     * generates an ISBN number.
     *
     * There are two types of ISBN numbers:
     *
     * - Base 13 Numbers are used since 2006. So they will be generated by default.
     * - Base 10 ISBNs were used until about 2007.
     *
     * @param base the base of the ISBN number to be generated (either 10 or 13). Defaults to 13.
     */
    def isbn(base: Int = 13): String = if (base == 13) generateBase13ISBN else generateBase10ISBN

    private def generateBase10ISBN = {
      val num = numerify("#" * 9)
      val remainder = sum(num, (idx, value) => idx * value) % 11
      val check = if (remainder == 10) "X" else remainder.toString
      s"$num$check"
    }

    private def generateBase13ISBN = {
      val num = numerify("#" * 12)
      val remainder = sum(num, (idx, value) => if (idx % 2 == 1) value else value * 3) % 10
      val check = (10 - remainder) % 10
      s"$num$check"
    }

    private def sum(num: String, sumFun: (Int, Int) => Int) = {
      val values = num.map(c => c - '0')
      val zipped = (1 to num.length).zip(values)
      zipped.foldLeft(0) {
        case (summed, (idx, value)) =>
          summed + sumFun(idx, value)
      }
    }
  }

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
    def color(implicit locale: FakerLocale = FakerLocale.default) = parse("commerce.color")

    def department(implicit locale: FakerLocale = FakerLocale.default) = parse("commerce.department")

    def productName(implicit locale: FakerLocale = FakerLocale.default) = parse("commerce.product_name.adjective") + " " + parse("commerce.product_name.material") + " " + parse("commerce.product_name.product")

    def price(implicit locale: FakerLocale = FakerLocale.default) = (Random.nextDouble * 100 * 100).floor / 100.0
  }

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

    /**
     * Generates a valid credit card expiry date. I.e. the expiry date is at least
     * the 1st of the next month.
     *
     * @return the 1st of a month in the future
     */
    def expiryDate: Date = {
      val c = Calendar.getInstance
      val year = c.get(Calendar.YEAR)
      val month = c.get(Calendar.MONTH)
      c.clear()
      c.set(Calendar.DAY_OF_MONTH, 1)
      c.set(Calendar.MONTH, month)
      c.set(Calendar.YEAR, year)
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
      val c = Calendar.getInstance
      val year = c.get(Calendar.YEAR)
      val month = c.get(Calendar.MONTH)
      c.clear()
      c.set(Calendar.DAY_OF_MONTH, 1)
      c.set(Calendar.MONTH, month)
      c.set(Calendar.YEAR, year)
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
    def email(implicit locale: FakerLocale = FakerLocale.default) = s"${userName()}@${domainName()}"

    /**
     * generates a persons email address at a free email provider (yahoo, gmail and the like)
     */
    def freeEmail(implicit locale: FakerLocale = FakerLocale.default) = {
      val d = parse("internet.free_email")
      s"$userName@$d"
    }

    /**
     * generates a persons email address at one of the sites example.[com,org,net]
     */

    def safeEmail(implicit locale: FakerLocale = FakerLocale.default) = {
      val domain = Array("com", "org", "net").randomElement
      s"$userName@example.$domain"
    }

    /**
     * generates the domain word from a generated companies name
     */
    def domainWord(implicit locale: FakerLocale = FakerLocale.default) = Company.name.split(" ").head.removeNonWordChars.toLowerCase

    /**
     * generates a domain suffix (com, org, net, ...) without leading period
     */
    def domainSuffix(implicit locale: FakerLocale = FakerLocale.default) = parse("internet.domain_suffix")

    /**
     * generates a password
     */
    def password(implicit locale: FakerLocale = FakerLocale.default) = Lorem.words().mkString

    /**
     * generates a mac address like `44:d5:ee:d0:f7:fb`
     */
    def macAddress(prefix: String = "") = {
      val prefixDigits = prefix.split(":").filterNot(_.isEmpty).map(s => Integer.parseInt(s, 16))
      val addressDigits = (1 to (6 - prefixDigits.size)).map(_ => Random.nextInt(256))
      (prefixDigits ++ addressDigits).map(i => f"$i%02x").mkString(":")
    }

    /**
     * generates an ip address like `142.246.100.144`
     */
    def ipV4Address = {
      val ary = 2 to 254
      Array(ary.randomElement, ary.randomElement, ary.randomElement, ary.randomElement).mkString(".")
    }

    /**
     * generates an IP V6 address like `77e2:40cb:3d2a:5697:d6cf:43ae:5cae:e343`
     */
    def ipV6Address = {
      val ary = 0 to 65535
      (1 to 8).map(_ => f"${ary.randomElement}%x").mkString(":")
    }
  }

  /**
   * Generates Lorem Ipsum related fake data
   */

  object Lorem extends Base {
    /**
     * generates a single word
     */
    def word(implicit locale: FakerLocale = FakerLocale.default) = parse("lorem.words")

    /**
     * generates a list of words
     * @param num the number of words to generate
     * @param supplemental `true` =&gt; use an additional set of words (not just the original lorem ipsum set)
     */
    def words(num: Int = 3, supplemental: Boolean = false)(implicit locale: FakerLocale = FakerLocale.default) = {
      val word_list = if (supplemental) get("lorem.words").get ++ get("lorem.supplemental").get else get("lorem.words").get
      word_list.sample(num)
    }

    /**
     * generates a string of random characters. The character set contains lower case letters and digits.
     * @param count the number of random chars to generate
     */
    def characters(count: Int = 255) = {
      val chars = ('a' to 'z') ++ ('0' to '9')
      chars.sample(count).mkString
    }

    /**
     * generate a sentence of lorem ipsum words.
     * @param wordCount the minimum number of words in the sentence
     * @param supplemental `true` =&gt; use an additional set of words (not just the original lorem ipsum set)
     * @param randomWordsToAdd the maximum number of words to add (wordCount + rand(randomWordsToAdd) will be generated.
     */
    def sentence(wordCount: Int = 4, supplemental: Boolean = false, randomWordsToAdd: Int = 6)(implicit locale: FakerLocale = FakerLocale.default) =
      words(wordCount + Random.nextInt(randomWordsToAdd), supplemental).map(s => s.head.toUpper + s.tail).mkString(" ") + "."

    /**
     * Generates a list of sentences
     * @param sentenceCount the number of sentences to generate.
     * @param supplemental `true` =&gt; use an additional set of words (not just the original lorem ipsum set)
     */
    def sentences(sentenceCount: Int = 3, supplemental: Boolean = false)(implicit locale: FakerLocale = FakerLocale.default) =
      (1 to sentenceCount).map(_ => sentence(wordCount = 3, supplemental = supplemental))

    /**
     * Generates a single paragraph string consisting of a number of sentences.
     *
     * @param sentenceCount the minimum number of sentences to generate.
     * @param supplemental `true` =&gt; use an additional set of words (not just the original lorem ipsum set)
     * @param randomSentencesToAdd the max. number of sentences to generate in addition to `sentenceCount`
     */
    def paragraph(sentenceCount: Int = 3, supplemental: Boolean = false, randomSentencesToAdd: Int = 3)(implicit locale: FakerLocale = FakerLocale.default) =
      sentences(sentenceCount + Random.nextInt(randomSentencesToAdd), supplemental).mkString(" ")

    /**
     * Generates a list of paragraphs
     * @param paragraphCount the number of paragraphs to generate
     * @param supplemental `true` =&gt; use an additional set of words (not just the original lorem ipsum set)
     */
    def paragraphs(paragraphCount: Int = 3, supplemental: Boolean = false)(implicit locale: FakerLocale = FakerLocale.default) =
      (1 to paragraphCount).map(_ => paragraph(supplemental = supplemental))
  }

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
    def name(implicit locale: FakerLocale = FakerLocale.default) = parse("name.name")

    def firstName(implicit locale: FakerLocale = FakerLocale.default) = parse("name.first_name")

    def lastName(implicit locale: FakerLocale = FakerLocale.default) = parse("name.last_name")

    /**
     * Generates a names prefix like 'Mr.' or an academic title (depending on the locale)
     */
    def prefix(implicit locale: FakerLocale = FakerLocale.default) = parse("name.prefix")

    /**
     * Generates a names suffix like 'MD' or 'III' (depending on the locale)
     */
    def suffix(implicit locale: FakerLocale = FakerLocale.default) = parse("name.suffix")

    /**
     * Generate a buzzword-laden job title
     *
     * Wordlist from http://www.bullshitjob.com/title/
     */
    def jobTitle(implicit locale: FakerLocale = FakerLocale.default) = parse("name.title.descriptor") + " " + parse("name.title.level") + " " + parse("name.title.job")
  }

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

  /**
   * Generates phone numbers
   * {{{
   * scala> Faker.PhoneNumber.phoneNumber
   * res32: String = (683)319-1686
   *
   * scala> Faker.PhoneNumber.cellPhone(Faker.FakerLocale.DE)
   * res34: String = +49-1656-50459299
   * }}}
   */
  object PhoneNumber extends Base {
    /**
     * Generates a random phone number in various formats.
     */
    def phoneNumber(implicit locale: FakerLocale = FakerLocale.default) = numerify(parse("phone_number.formats"))

    /**
     * generates a cell phone number. Depending on the locale the cell phone number might not be defined. In this
     * case the method will fall back to ordinary phone numbers.
     */
    def cellPhone(implicit locale: FakerLocale = FakerLocale.default) = parseSafe("cell_phone.formats").map(s => numerify(s)).getOrElse(phoneNumber)
  }

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
}