package com.walmart.adventofcode.y2016

import org.specs2.mutable.Specification

/**
  * Specifications for Day 2 Challenges.
  */
class Day2Specification extends Specification {

  "Advent of Code - Day 2" >> {

    "First Challenge: " >> {
      Day2.bathroomCode("ULL\nRRDDD\nLURDL\nUUUUD") must_== "1985"
    }

    "Second Challenge" >> {
      Day2.bathroomCode2("ULL\nRRDDD\nLURDL\nUUUUD") must_== "5DB3"
    }
  }

}
