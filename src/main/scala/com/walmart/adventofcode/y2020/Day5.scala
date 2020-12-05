package com.walmart.adventofcode.y2020

import scala.annotation.tailrec
import scala.io.Source._

object Day5 extends App {

  val input = fromResource("day5.txt").getLines().toList

  println(first())
  println(second())

  def first() = {
    input.map(stringToSeatNumber).max
  }

  def second(): Int = {
    var set = Set.range(0, first() + 1)
    input.map(stringToSeatNumber).foreach(x => set -= x)
    set.max
  }


  private def stringToSeatNumber(entry: String) = {
      recurse(0, 127, entry.toList.take(7), x => x.equals('B')) * 8 +
        recurse(0, 7, entry.toList.takeRight(3), x => x.equals('R'))
  }

  @tailrec
  def recurse(lowBound: Int, upperBound: Int, group: List[Char], isUpperHalf : Char => Boolean): Int = {
    if (group.isEmpty) if (lowBound == upperBound) lowBound else throw new Error("Answer not found")
    else {
      if (isUpperHalf(group.head)) recurse((upperBound + 1 + lowBound) / 2, upperBound, group.tail, isUpperHalf)
      else recurse(lowBound, (lowBound + upperBound) / 2, group.tail, isUpperHalf)
    }
  }
}
