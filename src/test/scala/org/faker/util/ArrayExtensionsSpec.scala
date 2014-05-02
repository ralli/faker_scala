package org.faker.util

import org.scalatest.{Matchers, FlatSpec}

class ArrayExtensionsSpec extends FlatSpec with Matchers with ArrayExtensions {
   "randomElement" should "return an element of the array" in {
     val arr = Array(1,2,3)
     val result = arr.randomElement
     arr should contain(result)
   }

}
