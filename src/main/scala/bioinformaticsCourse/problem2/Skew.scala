package bioinformaticsCourse.problem2

case class Skew(s: String) {
  def getSkew: List[Int] = {
    val c = s.toList

    def aux(g: List[Char], count: List[Int], ongoing: Int): List[Int] = g match {
      case Nil => count
      case x :: tail =>
        if (x == 'G') aux(tail, (1 + ongoing) :: count, ongoing + 1)
        else if (x == 'C') aux(tail, (ongoing - 1) :: count, ongoing - 1)
        else aux(tail, ongoing :: count, ongoing)

    }

    0 :: aux(c, List(), 0).reverse
  }
}

object SkewTester extends App {
  val s = "CATGGGCATCGGCCATACGCC"

  println(Skew(s).getSkew)
}