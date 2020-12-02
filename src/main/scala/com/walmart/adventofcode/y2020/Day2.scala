package com.walmart.adventofcode.y2020

import scala.io.Source._

object Day2 extends App {
  val input = fromResource("day2.txt").getLines.map(_.split(' ').toList).toList

  println(first())
  println(second())

  def first(): Int = {
    input.map(getPassword).count(password => {
      val count = password.password.count(password.character.equals)
      if (count >= password.firstNumber && count <= password.secondNumber) true
      else false
    })
  }

  def second(): Int = {
    input.map(getPassword).count(password => {
      def isBothSameCondition(equality: Boolean) = {
        (password.password(password.firstNumber-1) == password.character).equals(equality) &&
        (password.password(password.secondNumber-1) == password.character).equals(equality)
      }

      if (isBothSameCondition(true) || isBothSameCondition(false)) false
      else true
    })
  }

  def getPassword(line: List[String]) = {
    val bounds = line.head.split("-")
    new Password(bounds(0).toInt, bounds(1).toInt, line(1)(0), line(2))
  }

  class Password(val firstNumber:Int, val secondNumber: Int, val character : Char, val password: String)
}
