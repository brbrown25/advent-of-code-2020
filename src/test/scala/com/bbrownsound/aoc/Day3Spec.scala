package com.bbrownsound.aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Spec extends AnyFlatSpec with Matchers {
  val input = List(
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"
  )

  "Part1" should "properly count trees encountered" in {
    Day3.processInput(input, 3, 1) shouldBe 7
  }

  "Part2" should "properly count trees encountered and multiple" in {
    Day3.processInput(input, 1, 1) shouldBe 2
    Day3.processInput(input, 3, 1) shouldBe 7
    Day3.processInput(input, 5, 1) shouldBe 3
    Day3.processInput(input, 7, 1) shouldBe 4
    Day3.processInput(input, 1, 2) shouldBe 2
    List(
      Day3.processInput(input, 1, 1),
      Day3.processInput(input, 3, 1),
      Day3.processInput(input, 5, 1),
      Day3.processInput(input, 7, 1),
      Day3.processInput(input, 1, 2)
    ).fold(1)(_ * _) shouldBe 336
  }
}
