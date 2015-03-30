package rosalind.problem1

import org.scalatest._

/**
 * Created by hamishdickson on 30/03/15.
 *
 */
class CountingDnaNucleotidesTest extends FlatSpec with Matchers {
  "A dna counter" should "return the correct number of nucleotides" in {
    val dnaString: String = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    
    val counter: CountingDnaNucleotides = new CountingDnaNucleotides()
    
    counter.count("A", dnaString) should be (20)
  }
}
