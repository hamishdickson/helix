package com.hamishdickson.helix.rna

import com.hamishdickson.helix.protein.Protein
import org.scalatest.{FlatSpec, Matchers}

class RnaGenomeTest extends FlatSpec with Matchers {
  "An rna => protein reader" should "correctly use the codon table to translate an rna string to a protein string" in {
    val s: RnaGenome = Rna("AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")
    val p: List[Protein] = Protein.proteinString("MAMAPRTEINSTRING")

    //s.toProteinList should be (p)
  }
}
