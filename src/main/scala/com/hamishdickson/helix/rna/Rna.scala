package com.hamishdickson.helix.rna

import com.hamishdickson.helix.protein.Protein

trait Rna

case class RnaGenome(nucleotides: List[RnaNucleotide]) extends Rna {
  def toProteinList: List[Protein] = ???
}

object Rna {
  /**
   * Annoyingly, this data is typically stored as a string
   */
  def apply(s: String): RnaGenome = RnaGenome(s.toList.map(g => RnaNucleotide(g)))
}