package com.walmart.adventofcode.y2016

import org.specs2.mutable.Specification

/**
  * Specifications for Day 3 Challenges.
  */
class Day3Specification extends Specification {

  "Advent of Code - Day 3" >> {

    "First Challenge: " >> {
      Day3.triangleCount("5 10 25") must_== 0
    }

    "Second Challenge" >> {
      Day3.triangleCount("5 10 25") must_== 0
    }
  }

}
