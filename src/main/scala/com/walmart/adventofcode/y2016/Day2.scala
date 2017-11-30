package com.walmart.adventofcode.y2016

import scala.io.Source

/**
  * http://adventofcode.com/2016/day/2
  */
object Day2 {

  type KeyPosition = (Int, Int)

  /**
    * Day 2 - First Challenge
    */
  def bathroomCode(input: String) = {
    SquareKeypad.evaluateCode(input)
  }

  /**
    * Day 2 - Second Challenge
    */
  def bathroomCode2(input: String) = {
    DiamondKeypad.evaluateCode(input)
  }

  object SquareKeypad extends Keypad {
    override val keymap: Map[(Int, Int), Any] = Map (
      (-1, 1) -> 1, (0, 1) -> 2, (1, 1) -> 3,
      (-1, 0) -> 4, (0, 0) -> 5, (1, 0) -> 6,
      (-1, -1) -> 7, (0, -1) -> 8, (1, -1) -> 9
    )

    override val start: (Int, Int) = (0, 0)

    override def positionValidator(position: (Int, Int)): Boolean =
      if (Math.abs(position._1) > 1 || Math.abs(position._2) > 1) false
      else true
  }

  object DiamondKeypad extends Keypad {
    override val keymap: Map[(Int, Int), Any] = Map(
                                  (0, 2) -> 1,
                    (-1, 1) -> 2, (0, 1) -> 3, (1, 1) -> 4,
      (-2, 0) -> 5, (-1, 0) -> 6, (0, 0) -> 7, (1, 0) -> 8, (2, 0) -> 9,
                    (-1, -1) -> 'A',(0, -1) -> 'B', (1, -1) -> 'C',
                                  (0, -2) -> 'D'
    )
    override val start: (Int, Int) = (-2, 0)

    override def positionValidator(position: (Int, Int)): Boolean =
      if (Math.abs(position._1) + Math.abs(position._2) > 2) false
      else true
  }

  trait Keypad {
    val keymap: Map[KeyPosition, Any]
    val start: KeyPosition

    def evaluateCode(input: String) = {
      Source.fromString(input).getLines().foldLeft(start, "") {
        (result, line) => {
          val (keyPos, code) = result
          val newKeyPos = followInstructions(keyPos, line)
          (newKeyPos, code + keymap(newKeyPos))
        }
      }._2
    }

    private def followInstructions(keyPos: KeyPosition, line: String) =
      line.foldLeft(keyPos) {
        (result, instruction) => {
          nextKeyPositionEvaluator(result, instruction)
        }
      }

    private def nextKeyPositionEvaluator(curr: KeyPosition, instruction: Char) = {
      val newPos = instruction match {
        case 'U' => (curr._1, curr._2 + 1)
        case 'D' => (curr._1, curr._2 - 1)
        case 'L' => (curr._1 - 1, curr._2)
        case 'R' => (curr._1 + 1, curr._2)
        case _ => curr
      }
      if (positionValidator(newPos)) newPos
      else curr
    }

    def positionValidator(position: KeyPosition): Boolean

  }

  def main(args: Array[String]): Unit = {

    val input =
      """UULLULLUULLLURDLDUURRDRRLDURDULLRURDUDULLLUULURURLRDRRRRULDRUULLLLUUDURDULDRRDRUDLRRLDLUDLDDRURURUURRRDDDLLRUDURDULUULLRRULLRULDUDRDRLDLURURUDDUDLURUDUDURLURURRURLUDDRURRDLUURLLRURRDUDLULULUDULDLLRRRDLRDLDUDRDDDRRUURRRRRUURRDRRDLURDRRURDLLUULULLRURDLDDDRRLLRRUURULURUUDDLRRUDDRURUUDLRLRDLRURRRDULLDLRUDDUULRDULURUURDULUDLLRRLDDLRDLRUDRLDDRLRRRDURDULLRRRDRRLUURURDRRDRRLDLUDURURLDUURDRUDRDDRLDRRLDLURURULLUURUDUUDLRLL
        |LLLULLULDDULRLLURLLLRUUDDLRUULRLULLDLLRRDRLRLRLLDRUUURULDRDDLUDLLDUDULLLRLULLLRULDRDRUDLLRLRLLUDULRRRLDRUULDDULLDULULLUDUDLDRDURDLDLLDUDRRRDLUURRUURULLURLDURLRRLLDDUUULDRLUUDUDLURLULUDURRDRLLDDDDDRRULLRLDULULDDRUURRDLUDDDUDURDDRDRULULLLLUURDURUUUULUDLRURRULRDDRURURLLRLUUDUUURDLLDDLUDRLLLUDLLLLULRLURDRRRDUUDLLDLDDDURRDDRURUURDDRURRLDDDURDLLUURUUULRLUURRUDRLLDLURDUDRLULDLRLULULUDDLRDUDRUDLUULUULDURDRRRRLRULLUDRDDRDLDUDRDRRLDLLLLUDDLRULDLLDDUULDDRRULRRUURUDRDURLLLDDUUDRUUDLULLDR
        |UDUUULLDDDDLUDLDULRLRDLULLDDRULDURRLURRUDLRRUDURRDUDRRRUULRLLRLUDLDRRDUURDDRDRDUUUDUDLDLLRRLUURLUUUDDDUURLULURRLURRRDRDURURUDRLRUURUDRUDDDRDRDLDRDURDLDRRDUUDLLURLDDURRRLULDRDRLLRLLLRURLDURDRLDRUURRLDLDRLDDDRLDLRLDURURLLLLDDRDUDLRULULLRDDLLUDRDRRLUUULDRLDURURDUDURLLDRRDUULDUUDLLDDRUUULRRULDDUDRDRLRULUUDUURULLDLLURLRRLDDDLLDRRDDRLDDLURRUDURULUDLLLDUDDLDLDLRUDUDRDUDDLDDLDULURDDUDRRUUURLDUURULLRLULUURLLLLDUUDURUUDUULULDRULRLRDULDLLURDLRUUUDDURLLLLDUDRLUUDUDRRURURRDRDDRULDLRLURDLLRRDRUUUURLDRURDUUDLDURUDDLRDDDDURRLRLUDRRDDURDDRLDDLLRR
        |ULDRUDURUDULLUDUDURLDLLRRULRRULRUDLULLLDRULLDURUULDDURDUUDLRDRUDUDDLDRDLUULRRDLRUULULUUUDUUDDRDRLLULLRRDLRRLUDRLULLUUUUURRDURLLRURRULLLRLURRULRDUURRLDDRRDRLULDDRRDRLULLRDLRRURUDURULRLUDRUDLUDDDUDUDDUDLLRDLLDRURULUDRLRRULRDDDDDRLDLRRLUUDLUURRDURRDLDLDUDRLULLULRLDRDUDLRULLULLRLDDRURLLLRLDDDLLLRURDDDLLUDLDLRLUULLLRULDRRDUDLRRDDULRLLDUURLLLLLDRULDRLLLUURDURRULURLDDLRRUDULUURRLULRDRDDLULULRRURLDLRRRUDURURDURDULURULLRLDD
        |DURLRRRDRULDLULUDULUURURRLULUDLURURDDURULLRRUUDLRURLDLRUDULDLLRRULLLLRRLRUULDLDLLRDUDLLRLULRLLUUULULRDLDLRRURLUDDRRLUUDDRRUDDRRURLRRULLDDULLLURRULUDLRRRURRULRLLLRULLRRURDRLURULLDULRLLLULLRLRLLLDRRRRDDDDDDULUUDUDULRURDRUDRLUULURDURLURRDRRRRDRRLLLLUDLRRDURURLLULUDDLRLRLRRUURLLURLDUULLRRDURRULRULURLLLRLUURRULLLURDDDRURDUDDULLRULUUUDDRURUUDUURURRDRURDUDRLLRRULURUDLDURLDLRRRRLLUURRLULDDDUUUURUULDLDRLDUDULDRRULDRDULURRUURDU""".stripMargin

    println("Bath room code: " + bathroomCode(input))
    println("Bath room code 2: " + bathroomCode2(input))
  }

}
