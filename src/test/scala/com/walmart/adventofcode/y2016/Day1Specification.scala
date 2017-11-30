package com.walmart.adventofcode.y2016

import org.specs2.mutable.Specification

/**
  * Specifications for Day 1 Challenges.
  */
class Day1Specification extends Specification {

  "Advent of Code - Day 1" >> {

    "First Challenge: " >> {
      "Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away" >> {
        Day1.noOfBlocksAway("R2, L3") must_== 5
      }

      "R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away" >> {
        Day1.noOfBlocksAway("R2, R2, R2") must_== 2
      }

      "R5, L5, R5, R3 leaves you 12 blocks away" >> {
        Day1.noOfBlocksAway("R5, L5, R5, R3") must_== 12
      }
    }

    "Second Challenge: " >> {
      "R8, R4, R4, R8, the first location you visit twice is 4 blocks away, due East" >> {
        Day1.firstLocationVisitedTwice("R8, R4, R4, R8") must_== 4
      }
    }
  }

}
