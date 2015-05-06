package bioinformaticsCourse.problem1

class Freq {
  def merCounter(str: String, mers: Int): String = {
    val len = str.length

    def timesInString(tester: String): Int = {
      def times(pos: Int, count: Int): Int = {
        if (pos + mers > len) count
        else if (str.substring(pos, pos + mers) == tester) times(pos + 1, count + 1)
        else times(pos + 1, count)
      }

      times(0, 0)
    }

    def c(pos1: Int, max: Int, x: String): String = {
      if (pos1 + mers > len) x
      else if (timesInString(str.substring(pos1, pos1 + mers)) > max) c(pos1 + 1, timesInString(str.substring(pos1, pos1 + mers)), str.substring(pos1, pos1 + mers))
      // **cough** code smell **cough**
      else if (timesInString(str.substring(pos1, pos1 + mers)) == max && !x.split(" ").contains(str.substring(pos1, pos1 + mers))) c(pos1 + 1, timesInString(str.substring(pos1, pos1 + mers)), x + " " + str.substring(pos1, pos1 + mers))
      else c(pos1 + 1, max, x)
    }

    c(0, 0, "")
  }
}

// should be a unit test
object test2 extends App {
  //val str = "CGCCTAAATAGCCTCGCGGAGCCTTATGCCATACTCGTCCT"
  val str = "TAAACGTGAGAGAAACGTGCTGATTACACTTGTTCGTGTGGTAT"

  val freq = new Freq

  println(freq.merCounter(str, 3))
}
