package com.hamishdickson.helix

import com.hamishdickson.helix.RnaNucleotide

/**
 * This probably needs renaming after I spend a day on wikipedia
 */

trait Rna

case class RnaGenome(nucleotides: List[RnaNucleotide]) extends Rna

object Rna {
  def apply(s: String): RnaGenome = RnaGenome(s.toList.map(g => RnaNucleotide(g)))
}