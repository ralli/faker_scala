package org.faker

import org.scalatest.{FlatSpec, Matchers}

class AvatarSpec extends FlatSpec with Matchers with FakerBehaviors {

  "image" should behave like validResult(Avatar.image())
}
