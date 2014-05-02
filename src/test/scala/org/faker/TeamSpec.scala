package org.faker

import org.scalatest.{Matchers, FlatSpec}

class TeamSpec extends FlatSpec with Matchers with FakerBehaviors {

  "name" should behave like validResult(Team.name)
}
