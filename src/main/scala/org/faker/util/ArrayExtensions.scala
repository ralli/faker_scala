package org.faker.util

import scala.util.Random

trait ArrayExtensions {
  implicit class RandomInArray[T](coll: Array[T]) {
    def randomElement: T = coll(Random.nextInt(coll.size))
  }
}
