package com.hamishdickson.helix

/**
 * FIXME - I don't think Genome is the right word here, talk to a biologist at some point (or wikipedia...)
 *
 * This is identical to A Dna Genome, but with U instead of T
 */

sealed trait RnaNucleotide

case object RnaNucleotideA extends RnaNucleotide
case object RnaNucleotideG extends RnaNucleotide
case object RnaNucleotideC extends RnaNucleotide
case object RnaNucleotideU extends RnaNucleotide

object RnaNucleotide {
  def apply(c: Char) = c match {
    case 'A' => RnaNucleotideA
    case 'G' => RnaNucleotideG
    case 'C' => RnaNucleotideC
    case 'U' => RnaNucleotideU
  }
}
