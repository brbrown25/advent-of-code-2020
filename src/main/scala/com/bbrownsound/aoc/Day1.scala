package com.bbrownsound.aoc

import aocd.Problem

object Day1 extends Problem(2020, 1) {
  def run(input: List[String]): Unit = {
    val nums: List[Int] = input.map(_.toInt)

    println(twoSum(2020, nums))
    println(threeSum(2020, nums))
  }

  def keyByDiff(target: Int, numbers: List[Int]): Map[Int, Int] = {
    numbers.map(x => ((target - x) -> x)).toMap
  }

  def twoSum(target: Int, numbers: List[Int]): (List[Int], Int, Int) = {
    val diffMap = keyByDiff(target, numbers)
    val matches: List[Int] = numbers.flatMap { n => diffMap.get(n) }
    (matches, matches.fold(0)(_ + _), matches.fold(1)(_ * _))
  }

  def threeSum(target: Int, numbers: List[Int]): (List[Int], Int, Int) = {
    val diffMap = keyByDiff(target, numbers)
    val matches: List[Int] = numbers
      .flatMap { x => numbers.flatMap { y => diffMap.get(x + y).map(z => List(x, y, z).sorted) } }
      .flatten
      .distinct
    (matches, matches.fold(0)(_ + _), matches.fold(1)(_ * _))
  }
}
