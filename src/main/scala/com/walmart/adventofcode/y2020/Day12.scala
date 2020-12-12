package com.walmart.adventofcode.y2020

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day12 extends App {

  val input = fromResource("day12.txt").getLines().toList

  println(first(input, (0, 0, Direction.E)))

  @tailrec
  def first(input: List[String], state: (Int, Int, Direction.Value)): Int = {
    if (input.isEmpty) Math.abs(state._1) + Math.abs(state._2)
    else {
      val direction = input.head(0).toString
      val step = input.head.substring(1).toInt
      (direction) match {
        case "L" => first(input.tail, (state._1, state._2, Direction.leftRecursive(state._3, step / 90)))
        case "R" => first(input.tail, (state._1, state._2, Direction.rightRecursive(state._3, step / 90)))
        case _ =>
          val trueDirection = if (direction.equals("F")) state._3 else Direction.withName(direction)
          val move = Direction.move(trueDirection, step)
          first(input.tail, (state._1 + move._1, state._2 + move._2, state._3))
      }
    }
  }

  object Direction extends Enumeration {
    val W, N, E, S = Value

    @tailrec
    def leftRecursive(current: Direction.Value, howMany: Int): Value = {
      def left(current: Direction.Value) = {
        current match {
          case W => S
          case N => W
          case E => N
          case S => E
        }
      }

      if (howMany == 1) left(current)
      else leftRecursive(left(current), howMany - 1)
    }

    @tailrec
    def rightRecursive(current: Direction.Value, howMany: Int): Value = {
      def right(current: Direction.Value) = {
        current match {
          case W => N
          case N => E
          case E => S
          case S => W
        }
      }

      if (howMany == 1) right(current)
      else rightRecursive(right(current), howMany - 1)
    }

    def move(value: Direction.Value, step: Int) = {
      value match {
        case Direction.W => (0, -step)
        case Direction.E => (0, step)
        case Direction.N => (step, 0)
        case Direction.S => (-step, 0)
      }
    }
  }

}
