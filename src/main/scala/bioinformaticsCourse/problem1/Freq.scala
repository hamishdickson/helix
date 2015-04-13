package bioinformaticsCourse.problem1

/**
 * Created by hamishdickson on 13/04/15.
 *
 */
case class Freq(strand: String, mers: Int) {
  def merCounter = {

  }

  def getPatterns: List[String] = {
    
  }
}

object test extends App {
  val str = "CGCCTAAATAGCCTCGCGGAGCCTTATGCCATACTCGTCCT"

  println(Freq(str, 3).merCounter)
}
