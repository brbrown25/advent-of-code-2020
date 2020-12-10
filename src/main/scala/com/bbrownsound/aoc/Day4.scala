package com.bbrownsound.aoc

import aocd.Problem
import scala.util.matching._

object Day4 extends Problem(2020, 4) {
  val ValidEyeColors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  val ValidHairColorRE = """[0-9a-f]{6}""".r

  def validatePassports(passports: List[Passport], policy: Passport => (Boolean, Passport)): Int = {
    passports.map(policy).count(_._1 == true)
  }

  def passportValidation1(passport: Passport): (Boolean, Passport) = {
    (
      List(
        passport.byr,
        passport.iyr,
        passport.eyr,
        passport.hgt,
        passport.hcl,
        passport.ecl,
        passport.pid).flatten.size == 7,
      passport)
  }

  def passportValidation2(passport: Passport): (Boolean, Passport) = {
    val birthYearValidation = {
      passport.byr.map { by => by.length == 4 && by.toInt >= 1920 && by.toInt <= 2002 }
    }
    val issueYearValidation = {
      passport.iyr.map { iy => iy.length == 4 && iy.toInt >= 2010 && iy.toInt <= 2020 }
    }
    val expirationYearValidation = {
      passport.eyr.map { ey => ey.length == 4 && ey.toInt >= 2020 && ey.toInt <= 2030 }
    }
    val heightValidation = {
      passport.hgt.map { hg =>
        //these assume that the final chars are the uom
        hg.takeRight(2) match {
          case "cm" =>
            hg.dropRight(2).toInt >= 150 && hg.dropRight(2).toInt <= 193
          case "in" =>
            hg.dropRight(2).toInt >= 59 && hg.dropRight(2).toInt <= 76
          case _ => false
        }
      }
    }
    val hairColorValidation = {
      passport.hcl.map { hc =>
        hc.startsWith("#") && Day4.ValidHairColorRE.matches(hc.replace("#", ""))
      }
    }
    val eyeColorValidation = {
      passport.ecl.map(Day4.ValidEyeColors.contains(_))
    }
    val passportIdValidation = {
      passport.pid.map(_.size == 9)
    }
    val countryIdValidation = Some(true)
    val isValid = List(
      birthYearValidation,
      issueYearValidation,
      expirationYearValidation,
      heightValidation,
      hairColorValidation,
      eyeColorValidation,
      passportIdValidation,
      countryIdValidation
    ).flatten.count(identity) == 8

    (isValid, passport)
  }

  case class Passport(
      byr: Option[String] = None,
      iyr: Option[String] = None,
      eyr: Option[String] = None,
      hgt: Option[String] = None,
      hcl: Option[String] = None,
      ecl: Option[String] = None,
      pid: Option[String] = None,
      cid: Option[String] = None
  )

  def cleanInput(input: List[String]): List[String] = {
    input
      .map { i =>
        if (i.isEmpty) {
          "◊"
        } else {
          i
        }
      }
      .mkString(" ")
      .split("◊")
      .toList
      .map(_.trim)
  }

  def stringToPassport(s: String): Passport = {
    val kv = s
      .split(" ")
      .map { entry =>
        val Array(key, value) = entry.split(":")
        (key, value)
      }
      .toMap

    Passport(
      kv.get("byr"),
      kv.get("iyr"),
      kv.get("eyr"),
      kv.get("hgt"),
      kv.get("hcl"),
      kv.get("ecl"),
      kv.get("pid"),
      kv.get("cid"))
  }

  def run(input: List[String]): Unit = {
    val passports: List[Passport] = cleanInput(input).map(stringToPassport(_))

    //250
    println(validatePassports(passports, passportValidation1))
    //158
    println(validatePassports(passports, passportValidation2))
  }
}
