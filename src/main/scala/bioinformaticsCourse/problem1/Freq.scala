package bioinformaticsCourse.problem1

/**
 * Created by hamishdickson on 13/04/15.
 *
 *
 * rubbish didn't even read the question! this wants the most common 3-mer, not it's frequency
 */
class Freq {
  def merCounter(str: String, mers: Int): Int = {
    val len = str.length

    def timesInString(tester: String): Int = {
      def times(pos: Int, count: Int): Int = {
        if (pos + mers > len) count
        //println(str.substring(pos, pos + mers) + "\t" + tester + "\t" + count + "\t" + pos + "\t" + mers)
        else if (str.substring(pos, pos + mers) == tester) times(pos + 1, count + 1)
        else times(pos + 1, count)
      }

      times(0, 0)
    }

    def c(pos1: Int, max: Int): Int = {
      if (pos1 + mers > len) max
      else if (timesInString(str.substring(pos1, pos1 + mers)) > max) c(pos1 + 1, timesInString(str.substring(pos1, pos1 + mers)))
      else c(pos1 + 1, max)
    }

    c(0, 0)
  }
}

object test2 extends App {
  //val str = "CGCCTAAATAGCCTCGCGGAGCCTTATGCCATACTCGTCCT"
  val str = "ACAACTATGCATCACTATCGGGAACTATCCT"

  val freq = new Freq

  println(freq.merCounter(str, 5))
}
