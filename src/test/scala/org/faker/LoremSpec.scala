package org.faker

import org.scalatest.{FlatSpec, Matchers}
import org.faker.Faker.Lorem

class LoremSpec extends FlatSpec with Matchers with FakerBehaviors {
  import Faker.DefaultLocale._

  "word" should behave like validResult(Lorem.word)

  "words" should "not be empty" in {
    Lorem.words() should not be empty
  }

  "sentence" should behave like validResult(Lorem.sentence())
  it should "end with a period" in {
    Lorem.sentence() should endWith(".")
  }

  "sentences" should "not be empty" in {
    Lorem.sentences() should not be empty
  }

  "paragraph" should behave like validResult(Lorem.paragraph())

  "paragraphs" should "not be empty" in {
    Lorem.paragraphs() should not be empty
  }
}
