package bioinformaticsCourse.problem2

import scala.annotation.tailrec

case class Skew(s: String) {
  def getSkew: List[Int] = {
    val c = s.toList

    @tailrec
    def aux(g: List[Char], count: List[Int], ongoing: Int): List[Int] = g match {
      case Nil => count
      case x :: tail =>
        if (x == 'G') aux(tail, (1 + ongoing) :: count, ongoing + 1)
        else if (x == 'C') aux(tail, (ongoing - 1) :: count, ongoing - 1)
        else aux(tail, ongoing :: count, ongoing)

    }

    0 :: aux(c, List(), 0).reverse
  }

  /**
   * note: aware this could be nicer, but I'm still playing with pattern matching
   */
  def getMiniSkew: List[Int] = {
    @tailrec
    def mini(l: List[Int], minPos: List[Int], minVal: Int, curPos: Int): List[Int] = l match {
      case Nil => minPos
      case a :: tail =>
        if (a < minVal) mini(tail, List(curPos), a, curPos + 1)
        else if (a == minVal) mini(tail, curPos :: minPos, minVal, curPos + 1)
        else mini(tail, minPos, minVal, curPos + 1)
    }

    mini(getSkew, List(), 0, 0)
  }
}

object SkewTester extends App {
  val s = "CACTTATCTTGAACGTAA"

  println(Skew(s).getSkew)
  println(Skew(s).getMiniSkew)
}