package com.walmart.adventofcode.y2020

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day13 extends App {

  val input = prepareInput()

  println(first(input._1, input._1, input._2))

  def prepareInput(): (Int, List[Int]) = {
    val inputFileRead = fromResource("day13.txt").getLines().toList
    val busNumber = inputFileRead(1).split(",").filter(!_.equals("x")).map(_.toInt).toList
    (inputFileRead.head.toInt, busNumber)
  }

  @tailrec
  def first(initTs: Int, ts: Int, busNumbers: List[Int]): Int = {
    val output = busNumbers.filter(ts % _ == 0).toList
    if (output.isEmpty) first(initTs, ts + 1, busNumbers)
    else output(0) * (ts - initTs);
  }

}
