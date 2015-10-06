package com.hamishdickson.helix.protein

import com.hamishdickson.helix.rna.{Rna, RnaGenome}
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

  "The toProtein list" should "take a mRNA string and return a protein list" in {
    val s: String ="AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    val p: List[Protein] = Protein.proteinString("MAMAPRTEINSTRING")

    Codon.toProteinList(s) should be (p)
  }
}
