package com.hamishdickson.helix.dna

import com.hamishdickson.helix.rna.Rna
import org.scalatest.{FlatSpec, Matchers}

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

  "A subsequence finder" should "return an empty list if a subseq isn't in a genome" in {
    val s: Genome = Dna("GAGC")
    val t: Genome = Dna("T")

    s.subSequencePositions(t) should be (List())
  }

  it should "return an empty list if a subseq is longer than the genome" in {
    val s: Genome = Dna("T")
    val t: Genome = Dna("GAGC")

    s.subSequencePositions(t) should be (List())
  }

  it should "return an empty list if the subseq or the genome are empty" in {
    val s: Genome = Dna("")
    val t: Genome = Dna("GAGC")

    s.subSequencePositions(t) should be (List())

    val u: Genome = Dna("GAGC")
    val v: Genome = Dna("")

    u.subSequencePositions(v) should be (List())
  }

  it should "give position 1 if the subseq is the genome" in {
    val s: Genome = Dna("GAGC")
    val t: Genome = Dna("GAGC")

    s.subSequencePositions(t) should be (List(1))
  }

  it should "meet the example" in {
    val s: Genome = Dna("GATATATGCATATACTT")
    val t: Genome = Dna("ATAT")

    s.subSequencePositions(t) should be (List(2, 4, 10))
  }

  "A collection of genomes" should "yield a consensus genome" in {
    val g1: Genome = Dna("ATCCAGCT")
    val g2: Genome = Dna("GGGCAACT")
    val g3: Genome = Dna("ATGGATCT")
    val g4: Genome = Dna("AAGCAACC")
    val g5: Genome = Dna("TTGGAACT")
    val g6: Genome = Dna("ATGCCATT")
    val g7: Genome = Dna("ATGGCACT")

    val l: List[Genome] = List(g1, g2, g3, g4, g5, g6, g7)

    Dna.consensus(l) should be (Dna("ATGCAACT"))
  }
}
