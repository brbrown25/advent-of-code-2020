package com.bbrownsound.aoc

import aocd.Problem

object Day3 extends Problem(2020, 3) {
  def isTree(s: String): Boolean = {
    if (s == "#") true
    else false
  }

  def checkLine(line: String, idx: Int, multiplier: Int): Boolean = {
    val idxToCheck: Int = idx * multiplier
    val updatedLine: String = if (line.size <= idxToCheck) {
      line * idxToCheck
    } else {
      line
    }
    val stringToCheck: String = updatedLine.charAt(idxToCheck).toString
    isTree(stringToCheck)
  }

  def processInput(input: List[String], multiplier: Int, nextRow: Int): Int = {
    input.zipWithIndex
      .map {
        case (line: String, idx: Int) =>
          if (idx > 0 && idx % nextRow == 0) {
            checkLine(line, idx / nextRow, multiplier)
          } else {
            false
          }
      }
      .count(identity)
  }

  def run(input: List[String]): Unit = {
    //189
    println(processInput(input, 3, 1))
    //1718180100
    println(
      List(
        processInput(input, 1, 1),
        processInput(input, 3, 1),
        processInput(input, 5, 1),
        processInput(input, 7, 1),
        processInput(input, 1, 2)
      ).fold(1)(_ * _))
  }
}
