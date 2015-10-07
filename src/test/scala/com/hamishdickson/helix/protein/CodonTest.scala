package com.hamishdickson.helix.protein

import com.hamishdickson.helix.dna.{MRnaGenome, MRna}
import org.scalatest.{Matchers, FlatSpec}

class CodonTest extends FlatSpec with Matchers {
  "The Codon apply method" should "turn a list into a Codon" in {
    val c: List[Char] = List('U','U','U')

    Codon(c) should be (UUU)
  }

  "The toProtein method" should "take a codon and return a protein" in {
    val c: Codon = UUU

    Codon.toProtein(c) should be (ProteinF)
  }

  "The toProteinList method" should "take a mRNA string and return a protein list" in {
    val mRnaGenome: MRnaGenome = MRna("AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")
    val p: List[Protein] = Protein.proteinString("MAMAPRTEINSTRING")

    mRnaGenome.toProteinList should be (p)
  }

  "The toString method" should "take a protein list and return it's string counterpart" in {
    val mRnaGenome: MRnaGenome = MRna("AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")
    val r: String = "MAMAPRTEINSTRING"

    val ps: List[Protein] = mRnaGenome.toProteinList
    
    Protein.toStringFromList(ps) should be (r)
  }
}
