package org.faker.util

import scala.util.matching.Regex.Match

trait StringExtensions {

  implicit class StringExtensionMethods(string: String) {
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

    def removeNonWordChars = string.replaceAll( """\W""", "")
  }

}
