package com.walmart.adventofcode.y2016

/**
  * http://adventofcode.com/2016/day/1
  */
object Day1 {

  // x, y and facing. Ex:- (0, 0, N)
  type Position = (Int, Int, Char)

  // Ex:- (R, 2)
  type Instruction = (Char, Int)

  private val start = (0, 0, 'N')

  /**
    * Day 1 - First Challenge
    */
  def noOfBlocksAway(input: String) = {
    val finalPosition = input.split(",").foldLeft(start) {
      (result, instructionStr) => {
        nextPosition(result, parseInstruction(instructionStr))
      }
    }
    blocksAway(start, finalPosition)
  }

  /**
    * Day 1 - Second Challenge
    */
  def firstLocationVisitedTwice(input: String): Option[Position] = {
    input.split(",").foldLeft(Set(start), start) {
      (result, instructionStr) => {
        val (_, lastLocation) = result
        locationsVisited(lastLocation, parseInstruction(instructionStr)).foldLeft(result) {
          (partialResult, newLocation) => {
            val (uniqueLocations, _) = partialResult
            if (locationAlreadyVisited(uniqueLocations, newLocation)) return Some(newLocation)
            else (uniqueLocations + newLocation, newLocation)
          }
        }
      }
    }
    None
  }

  def parseInstruction(input: String): Instruction = {
    val trimmed = input.trim
    (trimmed.charAt(0), trimmed.substring(1).toInt)
  }

  def nextPosition(currPosition: Position, instruction: Instruction): Position = {
    val (x, y, facing) = currPosition
    val (direction, blocks) = instruction
    (facing, direction) match {
      case ('N', 'R') => (x + blocks, y, 'E')
      case ('N', 'L') => (x - blocks, y, 'W')
      case ('S', 'R') => (x - blocks, y, 'W')
      case ('S', 'L') => (x + blocks, y, 'E')
      case ('E', 'R') => (x, y - blocks, 'S')
      case ('E', 'L') => (x, y + blocks, 'N')
      case ('W', 'R') => (x, y + blocks, 'N')
      case ('W', 'L') => (x, y - blocks, 'S')
    }
  }

  def locationsVisited(currPosition: Position, instruction: Instruction): Seq[Position] = {
    val (x, y, facing) = currPosition
    val (direction, blocks) = instruction
    (facing, direction) match {
      case ('N', 'R') => for (i <- 1 to blocks) yield (x + i, y, 'E')
      case ('N', 'L') => for (i <- 1 to blocks) yield (x - i, y, 'W')
      case ('S', 'R') => for (i <- 1 to blocks) yield (x - i, y, 'W')
      case ('S', 'L') => for (i <- 1 to blocks) yield (x + i, y, 'E')
      case ('E', 'R') => for (i <- 1 to blocks) yield (x, y - i, 'S')
      case ('E', 'L') => for (i <- 1 to blocks) yield (x, y + i, 'N')
      case ('W', 'R') => for (i <- 1 to blocks) yield (x, y + i, 'N')
      case ('W', 'L') => for (i <- 1 to blocks) yield (x, y - i, 'S')
    }
  }

  private def locationAlreadyVisited(visited: Set[Position], location: Position) =
    visited.exists(p => p._1 == location._1 && p._2 == location._2 )

  private def blocksAway(start: Position, end: Position) = {
    Math.abs(start._1 - end._1) + Math.abs(start._2 - end._2)
  }

  def main(args: Array[String]): Unit = {
    val input = "R4, R4, L1, R3, L5, R2, R5, R1, L4, R3, L5, R2, L3, L4, L3, R1, R5, R1, L3, L1, R3, L1, R2, R2, L2, R5, L3, L4, R4, R4, R2, L4, L1, R5, L1, L4, R4, L1, R1, L2, R5, L2, L3, R2, R1, L194, R2, L4, R49, R1, R3, L5, L4, L1, R4, R2, R1, L5, R3, L5, L4, R4, R4, L2, L3, R78, L5, R4, R191, R4, R3, R1, L2, R1, R3, L1, R3, R4, R2, L2, R1, R4, L5, R2, L2, L4, L2, R1, R2, L3, R5, R2, L3, L3, R3, L1, L1, R5, L4, L4, L2, R5, R1, R4, L3, L5, L4, R5, L4, R5, R4, L3, L2, L5, R4, R3, L3, R1, L5, R5, R1, L3, R2, L5, R5, L3, R1, R4, L5, R4, R2, R3, L4, L5, R3, R4, L5, L5, R4, L4, L4, R1, R5, R3, L1, L4, L3, L4, R1, L5, L1, R2, R2, R4, R4, L5, R4, R1, L1, L1, L3, L5, L2, R4, L3, L5, L4, L1, R3"

    println(s"Easter Bunny HQ is ${Day1.noOfBlocksAway(input)} blocks away.")

    println(s"Blocks to first location visited twice: " + blocksAway(start, Day1.firstLocationVisitedTwice(input).get))
  }

}
