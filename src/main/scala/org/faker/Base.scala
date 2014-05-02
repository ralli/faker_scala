package org.faker

import scala.util.Random
import java.util.regex.Pattern
import org.faker.data.Data
import org.faker.util.{SeqExtensions, ArrayExtensions, StringExtensions}

trait Base
  extends StringExtensions
  with ArrayExtensions
  with SeqExtensions {
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
   * See [[org.faker.data.LocaleData.get( )]] for details.
   */
  def get(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[Seq[String]] = Data.get(key)

  /**
   * given a key returns a sequence of sequences to generate fake data.
   * See [[org.faker.data.LocaleData.getSeq( )]] for details.
   */
  def getSeq(key: String)(implicit locale: FakerLocale = FakerLocale.default): Option[Seq[Seq[String]]] = Data.getSeq(key)
}

object Base extends Base {}