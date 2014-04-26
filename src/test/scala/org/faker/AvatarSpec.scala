package org.faker

import org.scalatest.{FlatSpec, Matchers}
import org.faker.Faker.Avatar

class AvatarSpec extends FlatSpec with Matchers with FakerBehaviors {
  import Faker.DefaultLocale._

  "image" should behave like validResult(Avatar.image())
}
