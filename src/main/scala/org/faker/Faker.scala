package org.faker

import util.Random
import org.yaml.snakeyaml.Yaml
import scala.collection.JavaConverters._
import scala.annotation.tailrec
import java.util.regex.Pattern
import scala.util.matching.Regex.Match
import java.security.MessageDigest
import java.util.Date
import java.text.SimpleDateFormat

object Faker {

  private[faker] class Data(defaultLocale: String) {
    val fallbackLanguage = "en"
    val locales = (localesList(defaultLocale) :+ fallbackLanguage).distinct
    val localeDataMap = initLocaleDataMap

    def localesList(locale: String) = {
      val pattern = "^(\\w+)-.*$".r
      pattern.findFirstIn(locale) match {
        case Some(pattern(firstPart)) =>
          Vector(locale, firstPart)
        case _ =>
          Vector(locale)
      }
    }

    def initLocaleDataMap = locales.map {
      locale =>
        locale -> LocaleData(locale)
    }.toMap


    private def localeDataKey(locale: String, key: String) = s"$locale.faker.$key"

    def get(key: String): Option[Seq[String]] = {
      /*
       * the stream is evaluated lazily. So if a match is found in the 'de' locale,
       * the 'en' locale search will not be executed
       */
      val stream: Stream[Seq[String]] = for {
        locale <- locales.toStream
        localeData <- localeDataMap.get(locale)
        data <- localeData.get(localeDataKey(locale, key))
      } yield data
      stream.headOption
    }

    def getSeq(key: String): Option[Seq[Seq[String]]] = {
      /*
       * the stream is evaluated lazily. So if a match is found in the 'de' locale,
       * the 'en' locale search will not be executed
       */
      val stream: Stream[Seq[Seq[String]]] = for {
        locale <- locales.toStream
        localeData <- localeDataMap.get(locale)
        data <- localeData.getSeq(localeDataKey(locale, key))
      } yield data
      stream.headOption
    }
  }

  private[faker] case class LocaleData(locale: String) {
    lazy val yamlData = load(filePathOf(locale))

    def filePathOf(locale: String) = {
      s"/$locale.yml"
    }

    def load(name: String) = {
      val yaml = new Yaml()
      yaml.load(classOf[LocaleData].getResourceAsStream(name)).asInstanceOf[java.util.Map[String, Object]]
    }

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
    val data = new Data("en")

    def get(key: String): Option[Seq[String]] = data.get(key)

    def getSeq(key: String) = data.getSeq(key)
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

      def toSnakeCase = {
        gsub("^[A-Z]") {m : Match => m.toString.toLowerCase()}.gsub("""([A-Z])""") {m: Match => "_" + m.group(1).toLowerCase()}
      }

      def titlelize = string.gsub("(\\s+.)") { m => m.group(1).toUpperCase() }
    }

    implicit class RandomInArray[T](coll: Array[T]) {
      def rand: T = coll(Random.nextInt(coll.size))
    }

    implicit class RandomInSeq[T](coll: Seq[T]) {
      def rand: T = coll(Random.nextInt(coll.size))

      def sample(sizeOpt: Option[Int] = None): Seq[T] = sizeOpt.map {
        n => val result = (1 to n).map(i => coll(Random.nextInt(coll.size)))
          result
      }.getOrElse(Seq(rand))
    }

    val letters = ('a' to 'z') ++ ('A' to 'Z')

    private def randomDigit = (Random.nextInt(10) + '0').toChar

    def numerify(s: String) = {
      // make sure the first digit is not zero
      val s1 = "#".r.replaceFirstIn(s, (Random.nextInt(9) + 1).toString)
      s1.map(c => if (c == '#') randomDigit else c)
    }

    def letterify(s: String) = s.map(c => if (c == '?') letters.rand else c)

    def bothify(s: String) = letterify(numerify(s))


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
        gsub( """(\[[^\]]+\])\{(\d+),(\d+)\}""") {
        m: Match => m.group(1) * (m.group(2).toInt to m.group(3).toInt).rand
      }. // [12]{1,2} becomes [12] or [12][12]
        gsub( """(\([^\)]+\))\{(\d+),(\d+)\}""") {
        m: Match => m.group(1) * (m.group(2).toInt to m.group(3).toInt).rand
      }. // (12|34){1,2} becomes (12|34) or (12|34)(12|34)
        // TODO \d{1,3} does not work here and returns ddd instead of \d\d\d
        gsub( """(\\?.)\{(\d+),(\d+)\}""") {
        m: Match => m.group(1) * (m.group(2).toInt to m.group(3).toInt).rand
      }. // A{1,2} becomes A or AA
        gsub( """\((.*?)\)""") {
        m: Match => m.toString().gsub( """[\(\)]""", "").split('|').rand
      }. // (this|that) becomes 'this' or 'that'
        gsub( """\[([^\]]+)\]""") {
        m: Match => m.toString().gsub( """(\w)-(\w)""") {
          range: Match => (range.group(1)(0) to range.group(2)(0)).rand.toString
        }
      }. // All A-Z inside of [] become C (or X, or whatever)
        gsub( """\[([^\]]+)\]""") {
        m: Match => m.group(1).toVector.rand.toString
      }. // All [ABC] become B (or A or C)
        gsub( """\\d""")(m => ('0' to '9').rand.toString).
        gsub( """\\w""")(m => (('a' to 'z') ++ ('A' to 'Z')).rand.toString)
    }

    def extractSubKeys(key: String): List[String] = {
      val pattern = "#\\{([^}]+)\\}".r
      pattern.findAllMatchIn(key).map(m => m.group(1)).toList
    }

    def expandSubKey(baseKey: String, subKey: String): String = {
      if (subKey.contains('.')) {
        fetch(subKey.toSnakeCase).getOrElse(s"#{$subKey}")
      }
      else {
        val basePrefix = baseKey.split("\\.").init.mkString(".")
        fetch(s"$basePrefix.$subKey").getOrElse(s"#{$subKey}")
      }
    }

    def expandValues(key: String, value: String): String = {
      if (value.matches( """^/.*/$""")) {
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

    def fetch(key: String): Option[String] = get(key).map(seq => seq.rand)

    def parseSafe(key: String): Option[String] = fetch(key).map(value => expandValues(key, value))

    def parse(key: String): String = parseSafe(key).getOrElse(throw new NoSuchElementException(key))

    def get(key: String): Option[Seq[String]] = DataHolder.get(key)

    def getSeq(key: String) = DataHolder.getSeq(key)
  }

  object Address extends Base {
    def city = parse("address.city")

    def streetName = parse("address.street_name")

    def streetAddress(include_secondary: Boolean = false) = {
      val base = parse("address.street_address")
      val result = if (include_secondary) base + secondaryAddress else base
      numerify(result)
    }

    def secondaryAddress = numerify(parse("address.secondary_address"))

    def buildingNumber = bothify(parse("address.building_number"))

    def zipCode = bothify(parse("address.postcode"))

    def timeZone = bothify(parse("address.time_zone"))

    def zip = zipCode

    def postcode = zipCode

    def streetSuffix = parse("address.street_suffix")

    def citySuffix = parse("address.city_suffix")

    def cityPrefix = parse("address.city_prefix")

    def stateAbbr = parse("address.state_abbr")

    def state = parse("address.state")

    def country = parse("address.country")

    def latitude = (Random.nextDouble * 180 - 90).toString

    def longitude = (Random.nextDouble * 180 - 90).toString
  }

  object Avatar extends Base {
    def image(slug: Option[String] = None) = {
      // TODO use Lorem.words
      s"http://robohash.org/${slug.getOrElse(parse("lorem.words"))}}"
    }
  }

  object Bitcoin extends Base {
    def address: String = {
      val byteStream: Stream[Byte] = Stream.continually(Random.nextInt().toByte)
      val byteArray: Array[Byte] = (0.toByte #:: byteStream.take(20)).toArray
      val checkSum = sha2(byteArray).take(3)
      base58(byteArray ++ checkSum)
    }

    private def base58(a: Array[Byte]): String = {
      val alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".toArray
      val base = alphabet.length

      @tailrec
      def dec(n: BigInt, acc: List[Char]): String = {
        if (n == 0)
          acc.mkString
        else
          dec(n / base, alphabet(((n % base).toInt & 0xff) % base) :: acc)
      }

      dec(BigInt(a), Nil)
    }

    private def sha2(a: Array[Byte]): Array[Byte] = {
      val mac = MessageDigest.getInstance("SHA-256")
      mac.digest(a)
    }
  }

  object Business extends Base {
    def creditCardNumber: String = parse("business.credit_card_numbers")

    def creditCardExpiryDate: Date = new SimpleDateFormat("yyyy-MM-dd").parse(parse("business.credit_card_expiry_dates"))

    def creditCardType: String = parse("business.credit_card_types")
  }

  object Code extends Base {
    def isbn(base: Int = 10): String = if (base == 10) generateBase10ISBN else generateBase13ISBN

    private def generateBase10ISBN = {
      val num = numerify("#" * 9)
      val remainder = sum(num, (idx, value) => idx * value) % 11
      val check = if (remainder == 10) "X" else remainder.toString
      s"$num$check"
    }

    private def generateBase13ISBN = {
      val num = numerify("#" * 12)
      val remainder = sum(num, (idx, value) => if (value % 2 == 0) value else value * 3) % 10
      val check = 10 - remainder % 10
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

  object Commerce extends Base {
    def color = parse("commerce.color")

    def department = parse("commerce.department")

    def productName = parse("commerce.product_name.adjective") + " " + parse("commerce.product_name.material") + " " + parse("commerce.product_name.product")

    def price = (Random.nextDouble * 100 * 100).floor / 100.0
  }

  object Company extends Base {
    def name = parse("company.name")

    def suffix = parse("company.suffix")

    def catchPhrase = {
      val seqs = getSeq("company.buzzwords").get
      seqs.flatMap(s => s.sample()).mkString(" ")
    }

    def bs = {
      val seqs = getSeq("company.bs").get
      seqs.flatMap(s => s.sample()).mkString(" ")
    }

    def dunsNumber = numerify("##-###-####")
  }


  object Finance extends Base {
    val CREDIT_CARD_TYPES = Array("visa", "mastercard", "discover", "american_express", "diners_club", "jcb", "switch", "solo", "dankort", "maestro", "forbrugsforeningen", "laser")

    def creditCard(typesList: String*) = {
      val types: Vector[String] = if (typesList.isEmpty) CREDIT_CARD_TYPES.toVector else typesList.toVector
      val t = types.rand
      var template = numerify(parse(s"credit_card.$t"))
      var luhnDigit = (10 - (luhn(template) % 10)) % 10
      template.replaceAll("L", luhnDigit.toString)
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

  object Internet extends Base {
    def domainWord = Company.name.split(" ").head.replaceAll( """\W""", "").toLowerCase()

    def domainSuffix = parse("internet.domain_suffix")

    def macAddress(prefix: String = "") = {
      val prefixDigits = prefix.split(":").filterNot(_.isEmpty).map(s => Integer.parseInt(s, 16))
      val addressDigits = (1 to (6 - prefixDigits.size)).map(_ => Random.nextInt(256))
      (prefixDigits ++ addressDigits).map(i => f"$i%02x").mkString(":")
    }

    def ipV4Address = {
      var ary = 2 to 254
      Array(ary.rand, ary.rand, ary.rand, ary.rand).mkString(".")
    }

    def ipV6Address = {
      var ary = 0 to 65535
      (1 to 8).map(_ => f"${ary.rand}%x").mkString(":")
    }

    def fixUmlauts(str: String) = {
      """[äöüßÄÖÜ]""".r.replaceAllIn(str, m => m.toString() match {
        case "ä" => "ae"
        case "ö" => "oe"
        case "ü" => "ue"
        case "ß" => "ss"
        case "Ä" => "Ae"
        case "Ö" => "Oe"
        case "Ü" => "Ue"
      })
    }
  }

  object Lorem extends Base {
    def word = parse("lorem.words")

    def words(num: Int = 3, supplemental: Boolean = false) = {
      val word_list = if (supplemental) get("lorem.words").get ++ get("lorem.supplemental").get else get("lorem.words").get
      word_list.sample(Some(num))
    }

    def characters(count: Int = 255) = {
      val chars = ('a' to 'z') ++ ('0' to '9')
      chars.sample(Some(count)).mkString
    }

    def sentence(wordCount: Int = 4, supplemental: Boolean = false, randomWordsToAdd: Int = 6) =
      words(wordCount + Random.nextInt(randomWordsToAdd), supplemental).map(s => s.head.toUpper + s.tail).mkString(" ") + "."

    def sentences(sentenceCount: Int = 3, supplemental: Boolean = false) =
      (1 to sentenceCount).map(_ => sentence(wordCount = 3, supplemental = supplemental))

    def paragraph(sentenceCount: Int = 3, supplemental: Boolean = false, randomSentencesToAdd: Int = 3) =
      sentences(sentenceCount + Random.nextInt(randomSentencesToAdd), supplemental).mkString(" ")

    def paragraphs(paragraphCount: Int = 3, supplemental: Boolean = false) =
      (1 to paragraphCount).map(_ => paragraph(supplemental = supplemental))

  }

  object Name extends Base {
    def name = parse("name.name")

    def firstName = parse("name.first_name")

    def lastName = parse("name.last_name")

    def prefix = parse("name.prefix")

    def suffix = parse("name.suffix")

    /**
     * Generate a buzzword-laden job title
     *
     * Wordlist from http://www.bullshitjob.com/title/
     */
    def title = parse("name.title.descriptor") + " " + parse("name.title.level") + " " + parse("name.title.job")
  }

  object Number extends Base {
    def number(digits: Int) = numerify("#" * digits)
    def decimal(ldigits: Int, rdigits: Int =2) = numerify(("#" * ldigits) + '.' + ("#" * rdigits))
    def digit = Random.nextInt(10).toString
  }

  object PhoneNumber extends Base {
    def phoneNumber = numerify(parse("phone_number.formats"))
    def cellPhone = parseSafe("cell_phone.formats").map(s => numerify(s)).getOrElse(phoneNumber)
  }

  object Team extends Base {
    def name = parse("team.name").titlelize
    def creature = parse("team.creature")
    def state = parse("address.state")
  }
}