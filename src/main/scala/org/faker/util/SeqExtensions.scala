package org.faker.util

import scala.util.Random

trait SeqExtensions {
  implicit class RandomInSeq[T](coll: Seq[T]) {
    def randomElement: T = coll(Random.nextInt(coll.size))

    def sample(size: Int): Seq[T] = (1 to size).map(i => coll(Random.nextInt(coll.size)))
  }
}
