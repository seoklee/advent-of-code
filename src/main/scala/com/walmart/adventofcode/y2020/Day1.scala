package com.walmart.adventofcode.y2020

import scala.io.Source._
import scala.annotation.tailrec

object Day1 extends App {

  val input = fromResource("day1.txt").getLines().map(_.trim).map(_.toInt).toList

  println(first())
  println(second())
  
  def first(): Int = {
    firstRecursive(input.sorted)
  }
  
  // TODO fix this to accept arbitraily number of argument. can be n lg n
  @tailrec
  def firstRecursive(list: List[Int]): Int = {
    if (list.isEmpty) 0
    else {
      val value = list.tail.filter(_ + list.head == 2020).map(_ * list.head)
      if (value.isEmpty) firstRecursive(list.tail)
      // only one answer available
      else value(0)
    }
  }
  
  def second(): Int = {
    val sortedList = input.sorted
    for (i <- sortedList.indices) {
      val current = sortedList(i)
      var innerStart = i + 1
      var innerEnd = sortedList.length - 1
      
      while (innerStart < innerEnd) {
        val sum = current + sortedList(innerStart) + sortedList(innerEnd)
        
        sum match {
          case biggerSum if sum > 2020 => innerEnd = innerEnd - 1
          case lesserSum if sum < 2020 => innerStart = innerStart + 1
          case bingo if sum == 2020 => return current * sortedList(innerStart) * sortedList(innerEnd)
        }
      }
    }
    
    throw new Error("Solution not found!")
  }
}
