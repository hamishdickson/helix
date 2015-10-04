package com.hamishdickson.helix

import com.hamishdickson.helix.protein.Protein
import org.scalatest.{Matchers, FlatSpec}

class RnaGenomeTest extends FlatSpec with Matchers {
  "An rna => protein reader" should "correctly use the codon table to translate an rna string to a protein string" in {
    val s: RnaGenome = Rna("AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")
    val p: List[Protein] = Protein.proteinString("MAMAPRTEINSTRING")

    //s.toProteinList should be (p)
  }
}
