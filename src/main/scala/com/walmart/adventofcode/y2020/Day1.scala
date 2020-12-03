package com.walmart.adventofcode.y2020

import scala.io.Source._
import scala.annotation.tailrec

object Day1 extends App {

  val input = fromResource("day1.txt").getLines().map(_.trim).map(_.toInt).toList

  println(first())
  println(second())
  
  def first(): Int = {
    recurse(input.sorted, 2, 0)
  }

  def second(): Int = {
    recurse(input.sorted, 3, 0)
  }

  def recurse(remianingList: List[Int], count: Int, sum: Int): Int = {
    (remianingList, count, sum) match {
      case param if param._1.isEmpty || count < 2 => 0 // edge case
      case param if param._2 > 2 =>
        val withHead = recurse(remianingList.tail, count - 1, sum + remianingList.head)
        if (withHead != 0) {
          withHead * remianingList.head
        }
        else recurse(remianingList.head :: remianingList.tail.tail, count -1, sum + remianingList.tail.head) * remianingList.tail.head
      case param if param._2 == 2 =>
        val value = remianingList.tail.filter(sum + _ + remianingList.head == 2020).map(_ * remianingList.head)
        value match {
          case value if value.isEmpty => recurse(remianingList.tail, count, sum)
          case value if value.size == 1 => value.head
          case _ => throw new Error("No Answer found!")
        }
    }
  }

}
