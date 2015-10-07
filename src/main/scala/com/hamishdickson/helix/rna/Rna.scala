package com.hamishdickson.helix.rna

trait Rna

case class RnaGenome(nucleotides: List[RnaNucleotide]) extends Rna

case class MRnaGenome(nucleotides: List[RnaNucleotide]) extends Rna

object Rna {
  def apply(s: String): RnaGenome = RnaGenome(s.toList.map(g => RnaNucleotide(g)))
}