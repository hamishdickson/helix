package com.hamishdickson.helix

import com.hamishdickson.helix.protein.Protein

trait Rna

case class RnaGenome(nucleotides: List[RnaNucleotide]) extends Rna {
  def toProteinList: List[Protein] = List()
}

object Rna {
  def apply(s: String): RnaGenome = RnaGenome(s.toList.map(g => RnaNucleotide(g)))
}