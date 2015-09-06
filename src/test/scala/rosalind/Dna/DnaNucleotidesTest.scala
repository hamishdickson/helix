package rosalind.Dna

import org.scalatest.{Matchers, FlatSpec}

class DnaNucleotidesTest extends FlatSpec with Matchers {
  "A dna counter" should "return the correct number of nucleotides" in {
    val g: DnaNucleotides = Dna("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")

    g count Genome('A') should be (20)
    g count Genome('C') should be (12)
    g count Genome('G') should be (17)
    g count Genome('T') should be (21)
  }

  "An rna converter" should "be able to convert dna to rna" in {
    val g: DnaNucleotides = Dna("GATGGAACTTGACTACGTAAATT")
    val result: Rna = Rna("GAUGGAACUUGACUACGUAAAUU")

    g.convert should be (result)
  }

  "A reverse complementer" should "return the reverse complement of a nucleotide" in {
    val g: DnaNucleotides = Dna("AAAACCCGGT")
    val result: DnaNucleotides = Dna("ACCGGGTTTT")

    g.reverseComplementer should be (result)
  }

  "Rosalind_0808" should "spit out 50.51020408163265" in {
    val gc: DnaNucleotides = Dna("TGGTACAACCAGTATAAGAGGAGATAGTAGCGGCGACCATAGGCCATTGCTCAAGGTACATAACTGAGTTTGATGTTAGTTTCCGTCGAACATTCACTCTAGTCTTGGTATTGCGGGTGCCCTTGGCAACCGTCACCCTAGGCGAGGGGTATTGACGGTCCACGGAGTGGGTGTTAGCCTGGGAAAGCGCGGCCTTGCCGGTCAAGACTGCCCTACTCGATGATGATGCCCTATATTCAATCCTTTTAGGTACCGCGAACTTGTCGGCCTGCACCAGTTCAAGCCCCATAGTACGATCGGGCGTAGTTATTGGCCGTAGGGTGAAGGATCGGGTCCCCAAGGCTTAATGATGTGGAGTCCCCGTGTAGCTTTACACAGTGAGCCCTCACGACCAAGCAGCGATCAAGATTCGCCCATTGGTAATTGGGCGTTATATAGCGTTAGACCCCCTCTATCGTTGCCTCCAGAAGCAAAAGGCAAATTGGCTTTTGCACTCTAATAGTTAACGGGTAGCATATGCAAATTAGTCTTCCGCATCAAGCTAGCTGTAAATAATCTGACACGTCGGTGAACGTAATGTCCTGAGTTTGGTAGTACTCCTGACCAGATCTGGCAACAGCCGTATGCACATTTCTAACCTCAGAGGCACGACCTAATGCCCGGTCAGAACTTAAGGCGTGGAGGCCATTTCGTCACACATCCCCTTACGAGCGTCTAAATTCGAGGTCCCTCGTCTGCGGCCTGTAGTTTCTTATAGCATCGAAGAAGGTCGTTCCTCGCTGACTCCCATGAACCTCCCCCTGAGACTGGTCATCCAAGTATGGATATGTCAAATCACAAAGTTGACTGCTACGACGTTGTACAGTCAAGGTCGAAGCCAGTCAACTCTAGCCTATATAGGGACCCCTGTAGCTCACAGATAAACATAGTGGCTTTCATGCGGGTGCGAATAGAGCCCTAACTATGGACTCTGCCGTCAC")

    gc.cgCounter should be (50.51020408163265)
  }

  "A hemmingDistanceCounter" should "calculate the Hemming distance of 7 for the mutation" in {
    val s: DnaNucleotides = Dna("GAGCCTACTAACGGGAT")
    val t: DnaNucleotides = Dna("CATCGTAATGACGGCCT")

    s.hemmingDistance(t) should be (7)
  }

  "Given any two from the given set of k, m, n organisims mating, the function" should "output the prob their offspring with be dominant in the trait" in {
    val homozygousDominantPopulation = 2 // Yy
    val heterozygousDominantPopulation = 2 // yy
    val homozygousrecessivePopulation = 2 // er ...?

    val p: Double = Dna.getProbOfTrait(homozygousDominantPopulation, heterozygousDominantPopulation, homozygousrecessivePopulation)

    p should be (0.78333 +- 0.0001)
  }
}
