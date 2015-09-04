package rosalind.Dna

import org.scalatest.{Matchers, FlatSpec}

class DnaNucleotidesTest  extends FlatSpec with Matchers {
  "A dna counter" should "return the correct number of nucleotides" in {
    val genomeString: String = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"

    val genoneList: List[Genome] = genomeString.toList.map(g => Genome(g))

    val counter: DnaNucleotides = DnaNucleotides(genoneList)

    counter count Genome('A') should be (20)
    counter count Genome('C') should be (12)
    counter count Genome('G') should be (17)
    counter count Genome('T') should be (21)
  }

  "An rna converter" should "be able to convert dna to rna" in {
    val genoneString: String = "GATGGAACTTGACTACGTAAATT"
    val genoneResultString: String = "GAUGGAACUUGACUACGUAAAUU"

    val genoneList: List[Genome] = genoneString.toList.map(g => Genome(g))
    val genoneResultList: List[Genome] = genoneResultString.toList.map(g => Genome(g))

    val converter: DnaNucleotides = DnaNucleotides(genoneList)

    converter.convert should be (genoneResultList)
  }
}
