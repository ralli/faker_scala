package org.faker.util

import org.scalatest.{Matchers, FlatSpec}

class SeqExtensionsSpec extends FlatSpec with Matchers with SeqExtensions {
  "randomElement" should "return an element of the sequence" in {
    val seq = Vector(1,2,3,4)
    val result = seq.randomElement
    seq should contain(result)
  }

  "sample" should "return a sequence with a size the size argument passed" in {
    val seq = (1 to 100).toVector
    val size = 10
    val result = seq.sample(size)
    result should have size(size)
  }

  it should "return only elements which are elements of the original sequence" in {
    val seq = (1 to 100).toVector
    val size = 10
    val result = seq.sample(size)
    result foreach(e =>seq should contain(e))
  }
}
