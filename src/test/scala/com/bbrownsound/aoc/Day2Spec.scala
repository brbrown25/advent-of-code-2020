package com.bbrownsound.aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Spec extends AnyFlatSpec with Matchers {
  val input = List(
    "1-3 a: abcde",
    "1-3 b: cdefg",
    "2-9 c: ccccccccc"
  )

  def evaluatePassword(
      input: List[String],
      policy: (Int, Int, String, String) => Boolean): List[String] = {
    input.flatMap {
      case Day2.pattern(min, max, char, pw) =>
        val result = policy(min.toInt, max.toInt, char, pw)
        if (result) Some(s"$min-$max $char: $pw")
        else None
    }
  }

  "Part1" should "properly evaluate password policy 1" in {
    val validPasswords = evaluatePassword(input, Day2.policy1)
    Day2.validPasswords(input, Day2.policy1) shouldBe 2
    validPasswords shouldBe List("1-3 a: abcde", "2-9 c: ccccccccc")
  }

  "Part2" should "properly evaluate password policy 2" in {
    val validPasswords = evaluatePassword(input, Day2.policy2)
    Day2.validPasswords(input, Day2.policy2) shouldBe 1
    validPasswords shouldBe List("1-3 a: abcde")
  }
}
