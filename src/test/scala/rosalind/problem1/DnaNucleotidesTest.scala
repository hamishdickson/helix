package rosalind.problem1

import org.scalatest._
import rosalind.Dna.Genome

class DnaNucleotidesTest extends FlatSpec with Matchers {
  "A dna counter" should "return the correct number of nucleotides" in {
    val dnaString: String = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    
    val counter: DnaNucleotides = new DnaNucleotides(dnaString)
    
    counter.count('A') should be (20)
    counter.count('C') should be (12)
    counter.count('G') should be (17)
    counter.count('T') should be (21)

    counter.count3('A') should be (20)
    counter.count3('C') should be (12)
    counter.count3('G') should be (17)
    counter.count3('T') should be (21)

    val genoneList: List[Genome] = dnaString.toList.map(g => Genome(g))
  }
}
