package com.walmart.adventofcode.y2020

import scala.io.Source._
import scala.annotation.tailrec

object Day1 extends App {

  val input = fromResource("day1.txt").getLines().map(_.trim).map(_.toInt).toList

  println(first())
  println(second())
  
  def first(): Int = {
    recurse(input.sorted, 2, 0, List[Int]()).product
  }

  def second(): Int = {
    recurse(input.sorted, 3, 0, List[Int]()).product
  }

  @tailrec
  def recurse(remainingList: List[Int], count: Int, sum: Int, currentList: List[Int]): List[Int] = {
    (remainingList, count, sum) match {
      case param if param._1.isEmpty || count < 2 => Nil // edge case
      case param if param._2 > 2 =>
        val withHead = recurse(remainingList.tail, count - 1, sum + remainingList.head, remainingList.head :: currentList)
        withHead match {
          case withHead if withHead == Nil => Nil
          case _ =>  recurse(remainingList.head :: remainingList.tail.tail, count -1, sum + remainingList.tail.head, remainingList.tail.head :: currentList)
        }
      case param if param._2 == 2 =>
        val value = remainingList.tail.filter(sum + _ + remainingList.head == 2020)
        value match {
          case value if value.isEmpty => recurse(remainingList.tail, count, sum, currentList)
          case value if value.size == 1 => remainingList.head :: value.head :: currentList
          case _ => throw new Error("No Answer found!")
        }
    }
  }

}
