package rosalind.Dna

import org.scalatest.{Matchers, FlatSpec}

class GenomeTest extends FlatSpec with Matchers {
  "A dna counter" should "return the correct number of nucleotides" in {
    val g: Genome = Dna("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")

    g count Nucleotide('A') should be (20)
    g count Nucleotide('C') should be (12)
    g count Nucleotide('G') should be (17)
    g count Nucleotide('T') should be (21)
  }

  "An rna converter" should "be able to convert dna to rna" in {
    val g: Genome = Dna("GATGGAACTTGACTACGTAAATT")
    val result: Rna = Rna("GAUGGAACUUGACUACGUAAAUU")

    g.convert should be (result)
  }

  "A reverse complementer" should "return the reverse complement of a nucleotide" in {
    val g: Genome = Dna("AAAACCCGGT")
    val result: Genome = Dna("ACCGGGTTTT")

    g.reverseComplementer should be (result)
  }

  "Rosalind_0808" should "spit out 50.51020408163265" in {
    val gc: Genome = Dna("TGGTACAACCAGTATAAGAGGAGATAGTAGCGGCGACCATAGGCCATTGCTCAAGGTACATAACTGAGTTTGATGTTAGTTTCCGTCGAACATTCACTCTAGTCTTGGTATTGCGGGTGCCCTTGGCAACCGTCACCCTAGGCGAGGGGTATTGACGGTCCACGGAGTGGGTGTTAGCCTGGGAAAGCGCGGCCTTGCCGGTCAAGACTGCCCTACTCGATGATGATGCCCTATATTCAATCCTTTTAGGTACCGCGAACTTGTCGGCCTGCACCAGTTCAAGCCCCATAGTACGATCGGGCGTAGTTATTGGCCGTAGGGTGAAGGATCGGGTCCCCAAGGCTTAATGATGTGGAGTCCCCGTGTAGCTTTACACAGTGAGCCCTCACGACCAAGCAGCGATCAAGATTCGCCCATTGGTAATTGGGCGTTATATAGCGTTAGACCCCCTCTATCGTTGCCTCCAGAAGCAAAAGGCAAATTGGCTTTTGCACTCTAATAGTTAACGGGTAGCATATGCAAATTAGTCTTCCGCATCAAGCTAGCTGTAAATAATCTGACACGTCGGTGAACGTAATGTCCTGAGTTTGGTAGTACTCCTGACCAGATCTGGCAACAGCCGTATGCACATTTCTAACCTCAGAGGCACGACCTAATGCCCGGTCAGAACTTAAGGCGTGGAGGCCATTTCGTCACACATCCCCTTACGAGCGTCTAAATTCGAGGTCCCTCGTCTGCGGCCTGTAGTTTCTTATAGCATCGAAGAAGGTCGTTCCTCGCTGACTCCCATGAACCTCCCCCTGAGACTGGTCATCCAAGTATGGATATGTCAAATCACAAAGTTGACTGCTACGACGTTGTACAGTCAAGGTCGAAGCCAGTCAACTCTAGCCTATATAGGGACCCCTGTAGCTCACAGATAAACATAGTGGCTTTCATGCGGGTGCGAATAGAGCCCTAACTATGGACTCTGCCGTCAC")

    gc.cgCounter should be (50.51020408163265)
  }

  "A hemmingDistanceCounter" should "calculate the Hemming distance of 7 for the mutation" in {
    val s: Genome = Dna("GAGCCTACTAACGGGAT")
    val t: Genome = Dna("CATCGTAATGACGGCCT")

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
