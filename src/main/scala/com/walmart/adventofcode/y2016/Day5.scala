package com.walmart.adventofcode.y2016

import java.security.MessageDigest

/**
  * http://adventofcode.com/2016/day/5
  */
object Day5 {

  /**
    * Challenge 1
    */
  def password(doorId: String) = {
    Stream.from(0)
      .map(c => md5(doorId + c))
      .filter(hash => hash.startsWith("00000"))
      .take(8)
      .map(s => s.charAt(5))
      .mkString
  }

  /**
    *
    */
  def password2(doorId: String): String = {
    Stream.from(0)
      .map(c => md5(doorId + c))
      .filter(hash => hash.startsWith("00000") && hash.charAt(5).isDigit)
      .map(s => (s.charAt(5).asDigit, s.charAt(6)))
      .foldLeft(Map[Int, Char]()) {
        (resultMap, value) => {
          val (i, c) = value
          if (resultMap.size == 8) return resultMap.mkString
          else if (i > 7) resultMap
          else if (resultMap.contains(i)) resultMap
          else resultMap + (i -> c)
          }
        }.mkString(",")

  }

  def md5(s: String) = MessageDigest.getInstance("MD5")
    .digest(s.getBytes)
    .map("%02x".format(_))
    .mkString

  class Password(val p: Int, val c: Char) extends Ordered[Password] {
    println(this.toString)

    def canEqual(other: Any): Boolean = other.isInstanceOf[Password]

    override def equals(other: Any): Boolean = other match {
      case that: Password =>
        (that canEqual this) &&
          p == that.p
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(p)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

    override def compare(that: Password): Int = this.p - that.p

    override def toString = s"($p, $c)"
  }

  def main(args: Array[String]): Unit = {
    println("Password: " + password("reyedfim"))
    //println("Password: " + password2("abc"))
  }

}
