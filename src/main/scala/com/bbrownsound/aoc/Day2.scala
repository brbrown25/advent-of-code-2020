package com.bbrownsound.aoc

import aocd.Problem

object Day2 extends Problem(2020, 2) {
  val pattern = """(\d+)-(\d+)\s([A-z,a-z]{1}):\s([A-z,a-z]+)""".r

  def run(input: List[String]): Unit = {
    //645
    println(validPasswords(input, policy1))
    //737
    println(validPasswords(input, policy2))
  }

  def validPasswords(input: List[String], policy: (Int, Int, String, String) => Boolean): Int = {
    input
      .map {
        case pattern(min, max, char, pw) =>
          policy(min.toInt, max.toInt, char, pw)
      }
      .count(identity) //.count(_ == true)
  }

  def policy1(min: Int, max: Int, char: String, pw: String): Boolean = {
    val occurence: Int = pw.count(_ == char.toCharArray.headOption.getOrElse("-1"))
    occurence >= min && occurence <= max
  }

  def policy2(position1: Int, position2: Int, char: String, pw: String): Boolean = {
    val chars = pw.toList
    List(
      chars(position1 - 1) == char.toCharArray.headOption.getOrElse("-1"),
      chars(position2 - 1) == char.toCharArray.headOption.getOrElse("-1")
    ).filter(identity).size == 1
  }
}
