package org.faker

import scala.collection.immutable.IndexedSeq
import scala.util.Random

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
  def words(num: Int = 3, supplemental: Boolean = false)(implicit locale: FakerLocale = FakerLocale.default): Seq[String] = {
    val word_list = if (supplemental) get("lorem.words").get ++ get("lorem.supplemental").get else get("lorem.words").get
    word_list.sample(num)
  }

  /**
   * generates a string of random characters. The character set contains lower case letters and digits.
   * @param count the number of random chars to generate
   */
  def characters(count: Int = 255): String = {
    val chars = ('a' to 'z') ++ ('0' to '9')
    chars.sample(count).mkString
  }

  /**
   * generate a sentence of lorem ipsum words.
   * @param wordCount the minimum number of words in the sentence
   * @param supplemental `true` =&gt; use an additional set of words (not just the original lorem ipsum set)
   * @param randomWordsToAdd the maximum number of words to add (wordCount + rand(randomWordsToAdd) will be generated.
   */
  def sentence(wordCount: Int = 4, supplemental: Boolean = false, randomWordsToAdd: Int = 6)(implicit locale: FakerLocale = FakerLocale.default): String =
    words(wordCount + Random.nextInt(randomWordsToAdd), supplemental).map(s => s.head.toUpper + s.tail).mkString(" ") + "."

  /**
   * Generates a list of sentences
   * @param sentenceCount the number of sentences to generate.
   * @param supplemental `true` =&gt; use an additional set of words (not just the original lorem ipsum set)
   */
  def sentences(sentenceCount: Int = 3, supplemental: Boolean = false)(implicit locale: FakerLocale = FakerLocale.default): Seq[String] =
    (1 to sentenceCount).map(_ => sentence(wordCount = 3, supplemental = supplemental))

  /**
   * Generates a single paragraph string consisting of a number of sentences.
   *
   * @param sentenceCount the minimum number of sentences to generate.
   * @param supplemental `true` =&gt; use an additional set of words (not just the original lorem ipsum set)
   * @param randomSentencesToAdd the max. number of sentences to generate in addition to `sentenceCount`
   */
  def paragraph(sentenceCount: Int = 3, supplemental: Boolean = false, randomSentencesToAdd: Int = 3)(implicit locale: FakerLocale = FakerLocale.default): String =
    sentences(sentenceCount + Random.nextInt(randomSentencesToAdd), supplemental).mkString(" ")

  /**
   * Generates a list of paragraphs
   * @param paragraphCount the number of paragraphs to generate
   * @param supplemental `true` =&gt; use an additional set of words (not just the original lorem ipsum set)
   */
  def paragraphs(paragraphCount: Int = 3, supplemental: Boolean = false)(implicit locale: FakerLocale = FakerLocale.default): Seq[String] =
    (1 to paragraphCount).map(_ => paragraph(supplemental = supplemental))
}
