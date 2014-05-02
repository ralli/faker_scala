package org.faker.data

import org.yaml.snakeyaml.Yaml
import scala.annotation.tailrec
import org.faker.FakerLocale
import scala.collection.JavaConverters._

case class LocaleData(locale: String) {
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
   * an example is [[org.faker.Faker.Company.catchPhrase]] which uses the key 'company.buzzwords' to generate
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


