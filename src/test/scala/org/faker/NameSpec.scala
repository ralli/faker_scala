package org.faker

import org.scalatest.{FlatSpec, Matchers}
import org.faker.Faker.Name

class NameSpec extends FlatSpec with Matchers with FakerBehaviors {
  import Faker.DefaultLocale._

  "name" should behave like validResult(Name.name)

  "firstName" should behave like validResult(Name.firstName)

  "lastName" should behave like validResult(Name.lastName)

  "prefix" should behave like validResult(Name.prefix)

  "suffix" should behave like validResult(Name.suffix)

  "title" should behave like validResult(Name.title)
}
