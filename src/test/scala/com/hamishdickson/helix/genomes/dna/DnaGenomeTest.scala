package com.hamishdickson.helix.genomes.dna

import com.hamishdickson.helix.genomes.rna.Rna
import org.scalatest.{FlatSpec, Matchers}

class DnaGenomeTest extends FlatSpec with Matchers {
  "A dna counter" should "return the correct number of nucleotides" in {
    val g: DnaGenome = Dna("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")

    g count Nucleotide('A') should be (20)
    g count Nucleotide('C') should be (12)
    g count Nucleotide('G') should be (17)
    g count Nucleotide('T') should be (21)
  }

  "An rna converter" should "be able to convert dna to rna" in {
    val g: DnaGenome = Dna("GATGGAACTTGACTACGTAAATT")
    val result: Rna = Rna("GAUGGAACUUGACUACGUAAAUU")

    g.convert should be (result)
  }

  "A reverse complementer" should "return the reverse complement of a nucleotide" in {
    val g: DnaGenome = Dna("AAAACCCGGT")
    val result: DnaGenome = Dna("ACCGGGTTTT")

    g.reverseComplementer should be (result)
  }

  "Rosalind_0808" should "spit out 50.51020408163265" in {
    val gc: DnaGenome = Dna("TGGTACAACCAGTATAAGAGGAGATAGTAGCGGCGACCATAGGCCATTGCTCAAGGTACATAACTGAGTTTGATGTTAGTTTCCGTCGAACATTCACTCTAGTCTTGGTATTGCGGGTGCCCTTGGCAACCGTCACCCTAGGCGAGGGGTATTGACGGTCCACGGAGTGGGTGTTAGCCTGGGAAAGCGCGGCCTTGCCGGTCAAGACTGCCCTACTCGATGATGATGCCCTATATTCAATCCTTTTAGGTACCGCGAACTTGTCGGCCTGCACCAGTTCAAGCCCCATAGTACGATCGGGCGTAGTTATTGGCCGTAGGGTGAAGGATCGGGTCCCCAAGGCTTAATGATGTGGAGTCCCCGTGTAGCTTTACACAGTGAGCCCTCACGACCAAGCAGCGATCAAGATTCGCCCATTGGTAATTGGGCGTTATATAGCGTTAGACCCCCTCTATCGTTGCCTCCAGAAGCAAAAGGCAAATTGGCTTTTGCACTCTAATAGTTAACGGGTAGCATATGCAAATTAGTCTTCCGCATCAAGCTAGCTGTAAATAATCTGACACGTCGGTGAACGTAATGTCCTGAGTTTGGTAGTACTCCTGACCAGATCTGGCAACAGCCGTATGCACATTTCTAACCTCAGAGGCACGACCTAATGCCCGGTCAGAACTTAAGGCGTGGAGGCCATTTCGTCACACATCCCCTTACGAGCGTCTAAATTCGAGGTCCCTCGTCTGCGGCCTGTAGTTTCTTATAGCATCGAAGAAGGTCGTTCCTCGCTGACTCCCATGAACCTCCCCCTGAGACTGGTCATCCAAGTATGGATATGTCAAATCACAAAGTTGACTGCTACGACGTTGTACAGTCAAGGTCGAAGCCAGTCAACTCTAGCCTATATAGGGACCCCTGTAGCTCACAGATAAACATAGTGGCTTTCATGCGGGTGCGAATAGAGCCCTAACTATGGACTCTGCCGTCAC")

    gc.cgCounter should be (50.51020408163265)
  }

  "A hemmingDistanceCounter" should "calculate the Hemming distance of 7 for the mutation" in {
    val s: DnaGenome = Dna("GAGCCTACTAACGGGAT")
    val t: DnaGenome = Dna("CATCGTAATGACGGCCT")

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
    val s: DnaGenome = Dna("GAGC")
    val t: DnaGenome = Dna("T")

    s.subSequencePositions(t) should be (List())
  }

  it should "return an empty list if a subseq is longer than the genome" in {
    val s: DnaGenome = Dna("T")
    val t: DnaGenome = Dna("GAGC")

    s.subSequencePositions(t) should be (List())
  }

  it should "return an empty list if the subseq or the genome are empty" in {
    val s: DnaGenome = Dna("")
    val t: DnaGenome = Dna("GAGC")

    s.subSequencePositions(t) should be (List())

    val u: DnaGenome = Dna("GAGC")
    val v: DnaGenome = Dna("")

    u.subSequencePositions(v) should be (List())
  }

  it should "give position 1 if the subseq is the genome" in {
    val s: DnaGenome = Dna("GAGC")
    val t: DnaGenome = Dna("GAGC")

    s.subSequencePositions(t) should be (List(1))
  }

  it should "meet the example" in {
    val s: DnaGenome = Dna("GATATATGCATATACTT")
    val t: DnaGenome = Dna("ATAT")

    s.subSequencePositions(t) should be (List(2, 4, 10))
  }

  "A collection of genomes" should "yield a consensus genome" in {
    val g1: DnaGenome = Dna("ATCCAGCT")
    val g2: DnaGenome = Dna("GGGCAACT")
    val g3: DnaGenome = Dna("ATGGATCT")
    val g4: DnaGenome = Dna("AAGCAACC")
    val g5: DnaGenome = Dna("TTGGAACT")
    val g6: DnaGenome = Dna("ATGCCATT")
    val g7: DnaGenome = Dna("ATGGCACT")

    val l: List[DnaGenome] = List(g1, g2, g3, g4, g5, g6, g7)

    Dna.consensus(l) should be (Dna("ATGCAACT"))
  }

  it should "yield a formatted consensus genome" in {
    val g1: DnaGenome = Dna("ATCCAGCT".replace("\n", ""))
    val g2: DnaGenome = Dna("GGGCAACT".replace("\n", ""))
    val g3: DnaGenome = Dna("ATGGATCT".replace("\n", ""))
    val g4: DnaGenome = Dna("AAGCAACC".replace("\n", ""))
    val g5: DnaGenome = Dna("TTGGAACT".replace("\n", ""))
    val g6: DnaGenome = Dna("ATGCCATT".replace("\n", ""))
    val g7: DnaGenome = Dna("ATGGCACT".replace("\n", ""))

    val l: List[DnaGenome] = List(g1, g2, g3, g4, g5, g6, g7)

    Dna.formattedConsensus(l) should be ("ATGCAACT\nA: 5 1 0 0 5 5 0 0\nC: 0 0 1 4 2 0 6 1\nG: 1 1 6 3 0 1 0 0\nT: 1 5 0 0 0 1 1 6")
  }
}
