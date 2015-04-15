package bioinformaticsCourse.problem1

/**
 * Created by hamishdickson on 13/04/15.
 */
class Counter {

  def count(text: String, pattern: String): Int = {
    def c(t: String, x: Int): Int = {
      if (t.length < pattern.length) x
      else if (t.substring(0, pattern.length) == pattern) c(t.substring(1), x + 1)
      else c(t.substring(1), x)
    }

    c(text, 0)
  }
}


object test extends App {
  val pattern = "ACTGTACGATGATGTGTGTCAAAG"
  val text = "TGT"

  val counter = new Counter

  println(counter.count(pattern, text))
 // println(counter.count("CGCCTAGTTGCCTAGTGACGCCTAGTCGTCGGCCTAGTCCGCCTAGTGCCTAGTTGCCTAGTGAAGCCTAGTATGCCTAGTGCCTAGTGCCTAGTTCTGCCTAGTGTTGCCTAGTACGCCTAGTAGCCTAGTACCAACGCCTAGTTGAATGCCTAGTGACGCCTAGTGCCTAGTAGCCTAGTACGCCTAGTAGCCTAGTCCTAAGCCTAGTACGGCCTAGTGACGCGCCTAGTACCTGCCTAGTCACCGCCTAGTCGCAGCCTAGTGCGCCTAGTCGCCTAGTTGCCTAGTGCCTAGTTCGCCTAGTGCTGCCTAGTGGATAGCCTAGTGCCTAGTAGGAGTCGAAGCGCCTAGTGGCCTAGTCGCCTAGTGCCTAGTATTGCCTAGTCATGCCTAGTCCCGGCCCAGATCTCCGGCCTAGTGCCTAGTGTCAGGCCTAGTACACACGCCTAGTGCCTAGTCCCGCCTAGTACGCCTAGTCTGCGCAAGCCTAGTGCCTAGTGGCCTAGTGGCCTAGTGCCTAGTATTTCCGCCTAGTGGCCTAGTAGGCCTAGTCTTGGCTGACAGCCTAGTTAGTGCCTAGTGGCCTAGTAGATATAAGGCCTAGTGCCTAGTGGCCTAGTATAAGATAGCCTAGTTCGCAGGACTTCCGCCTAGTTGTCTACCGCCTAGTCGGCCTAGTGCCTAGTAAGGCCTAGTCGTAGCCTAGTGCCTAGTGTGCCTAGTGCCTAGTGCCTAGTGCCTAGTTGCCTAGTCGCCTAGTTGGCCTAGTGCCTAGTAGCCTAGTAAAAGGCCTAGTGCCTAGTTGCCTAGTCATAGCCTAGTGACGCCTAGTATTCGCCTAGTGCGCCTAGTACCGCCTAGTGCCTAGTAGCCTAGTTAGTGTGCCTAGTGCCTAGTCAGCCTAGTGCCTAGTAGCCTAGTGCCTAGTCTGCCCGCCGCCTAGTCGCCTAGTTGCCTAGTTGTTGCGCCTAGTTACCCAGCCTAGTTCCAGCCTAGTTGCGCCTAGTTACGCCTAGTCGCCTAGTTGCCTAGTTG", "GCCTAGTGC"))
}