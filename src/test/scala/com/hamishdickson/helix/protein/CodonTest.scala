package com.hamishdickson.helix.protein

import com.hamishdickson.helix.genomes.rna.{MRna, MRnaGenome, RnaNucleotide, RnaNucleotideU}
import org.scalatest.{Matchers, FlatSpec}

class CodonTest extends FlatSpec with Matchers {
  "The Codon apply method" should "turn a list into a Codon" in {
    val c: List[RnaNucleotide] = List(RnaNucleotideU, RnaNucleotideU, RnaNucleotideU)

    Codon(c) should be (UUU)
  }

  "The toProtein method" should "take a codon and return a protein" in {
    val c: Codon = UUU

    c.toProtein should be (ProteinF)
  }

  "The toProteinList method" should "take a mRNA string and return a protein list" in {
    val mRnaGenome: MRnaGenome = MRna("AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")
    val p: ProteinChain = ProteinRope("MAMAPRTEINSTRING")

    mRnaGenome.toProteinList should be (p)
  }

  "The toString method" should "take a protein list and return it's string counterpart" in {
    val mRnaGenome: MRnaGenome = MRna("AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")
    val r: String = "MAMAPRTEINSTRING"

    val ps: ProteinChain = mRnaGenome.toProteinList

    ps.toString should be (r)
  }
}
