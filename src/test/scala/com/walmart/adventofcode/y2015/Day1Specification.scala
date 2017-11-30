package com.walmart.adventofcode.y2015

import org.specs2.mutable.Specification

/**
  * Specifications for Day 1 Challenges.
  */
class Day1Specification extends Specification {

  "Advent of Code - Day 1" >> {

    "First Challenge: " >> {
      "must remain on ground floor(0) with no input " >> {
        Day1.finalFloor("") must_== 0
      }
      "must be on floor 1 for input (" >> {
        Day1.finalFloor("(") must_== 1
      }
      "must be on floor -1 for input )" >> {
        Day1.finalFloor(")") must_== -1
      }
      "(()) and ()() both result in floor 0" >> {
        Day1.finalFloor("(())") must_== 0
        Day1.finalFloor("()()") must_== 0
      }
      "((( and (()(()( both result in floor 3" >> {
        Day1.finalFloor("(((") must_== 3
        Day1.finalFloor("(()(()(") must_== 3
      }
      "))((((( also results in floor 3" >> {
        Day1.finalFloor("))(((((") must_== 3
      }
      "()) and ))( both result in floor -1 (the first basement level)" >> {
        Day1.finalFloor("())") must_== -1
        Day1.finalFloor("))(") must_== -1
      }
      "))) and )())()) both result in floor -3" >> {
        Day1.finalFloor(")))") must_== -3
        Day1.finalFloor(")())())") must_== -3
      }
    }

    "Second Challenge: " >> {
      ") causes him to enter the basement at character position 1"  >> {
        Day1.firstPositionToVisitBasement(")") must_== Some(1)
      }
      "()()) causes him to enter the basement at character position 5" >> {
        Day1.firstPositionToVisitBasement("()())") must_== Some(5)
      }
      "((((( causes him to never visit the basement" >> {
        Day1.firstPositionToVisitBasement("(((((") must_== None
      }
    }
  }
}
