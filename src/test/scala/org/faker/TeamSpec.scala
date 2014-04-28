package org.faker

import org.scalatest.{Matchers, FlatSpec}
import org.faker.Faker.Team

class TeamSpec extends FlatSpec with Matchers with FakerBehaviors {

  "name" should behave like validResult(Team.name)

  "creature" should behave like validResult(Team.creature)

  "state" should behave like validResult(Team.state)
}
