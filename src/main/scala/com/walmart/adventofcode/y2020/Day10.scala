package com.walmart.adventofcode.y2020

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source._

object Day10 extends App {

  val input = fromResource("day10_test.txt").getLines().map(_.toInt).toList.sorted

  println(first())
  println(second(input, List((0, 1))))

  def first() = {
    val countList = mutable.Map[Int, Int]((1, 0), (2, 0), (3, 0));
    recurseFirst(input, countList, 0)
    countList(1) * (countList(3) + 1)
  }

  @tailrec
  def recurseFirst(input: List[Int], count: mutable.Map[Int, Int], state: Int): Unit = {
    if (input.nonEmpty) {
      val diff = input.head - state
      count(diff) = count(diff) + 1
      recurseFirst(input.tail, count, state + diff)
    }
  }

  @tailrec
  def second(input: List[Int], stateList: List[(Int, Long)]): Long = {
    if (input.isEmpty) stateList.head._2
    else {
      val newStateList = stateList.filter(tuple => tuple._1 >= input.head - 3)
      second(input.tail, (input.head, newStateList.map(tuple => tuple._2).sum) :: newStateList)
    }

  }

}
