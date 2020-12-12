package com.walmart.adventofcode.y2020

import java.util

import scala.io.Source._

object Day4 extends App{

  val input = prepareInput(fromResource("day4.txt").getLines().toList)

  println(first())
  println(second())

  // TODO make it tailrec
  def prepareInput(inputList: List[String]): List[List[String]] = {
    if (inputList.isEmpty) Nil
    else {
      val passport = inputList.takeWhile(_.nonEmpty)
      passport.flatMap(_.split(" ")) :: prepareInput(inputList.drop(passport.length + 1))
    }
  }

  def first() = {
    getValidPassports(input).size
  }

  def second(): Int = {
    val eyeColor = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

    def validate(field: String): Boolean = {
      def validateYear(value: String, lowerBound: Int, upperBound: Int): Boolean = {
        value.length == 4 && lowerBound <= value.toInt && value.toInt <= upperBound
      }

      def validateHeight(value: String): Boolean = {
        value match {
          case height if height.endsWith("cm") => height.dropRight(2).toInt >= 150 && height.dropRight(2).toInt <= 193
          case height if height.endsWith("in") => height.dropRight(2).toInt >= 59&& height.dropRight(2).toInt <= 76
          case _ => false
        }
      }

      def isCharacterNumber(x: Char): Boolean = {
        x >= '0' && x <= '9'
      }

      field.split(":") match {
        case fieldValue if fieldValue(0).equals("byr") => validateYear(fieldValue(1), 1920, 2002)
        case fieldValue if fieldValue(0).equals("iyr") => validateYear(fieldValue(1), 2010, 2020)
        case fieldValue if fieldValue(0).equals("eyr") => validateYear(fieldValue(1), 2020, 2030)
        case fieldValue if fieldValue(0).equals("hgt") => validateHeight(fieldValue(1))
        case fieldValue if fieldValue(0).equals("hcl") => fieldValue(1).length == 7 && fieldValue(1).startsWith("#") && fieldValue(1).toList.drop(1).count(x => isCharacterNumber(x) || (x.toUpper >= 'A' && x.toUpper <= 'Z')) == 6
        case fieldValue if fieldValue(0).equals("ecl") => eyeColor.contains(fieldValue(1))
        case fieldValue if fieldValue(0).equals("pid") => fieldValue(1).length == 9 && fieldValue(1).toList.count(isCharacterNumber) == 9
        case _ => true
      }
    }

    getValidPassports(input).map(entries => {
      val i = entries.count(validate)
      if (i == entries.size) 1 else 0
    }).sum
  }

  def getValidPassports(input: List[List[String]]): List[List[String]] = {
    input.filter(_.size > 6)
      .filter(entry => !entry.map(_.split(":").head).toSet.contains("cid") || entry.size == 8);
  }

}
