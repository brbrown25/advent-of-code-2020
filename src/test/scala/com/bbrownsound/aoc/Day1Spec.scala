package com.bbrownsound.aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {
  val input = List(
    1721, 979, 366, 299, 675, 1456
  )

  "Part1" should "properly calculate 2 sum" in {
    val (nums, add, mult) = Day1.twoSum(2020, input)

    nums shouldBe List(299, 1721)
    add shouldBe 2020
    mult shouldBe 514579
  }

  "Part2" should "properly calculate 3 sum" in {
    val (nums, add, mult) = Day1.threeSum(2020, input)

    nums shouldBe List(366, 675, 979)
    add shouldBe 2020
    mult shouldBe 241861950
  }
}
