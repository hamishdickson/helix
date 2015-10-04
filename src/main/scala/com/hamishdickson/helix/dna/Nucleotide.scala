package com.hamishdickson.helix.dna

sealed trait Nucleotide

case object NucleotideT extends Nucleotide
case object NucleotideA extends Nucleotide
case object NucleotideG extends Nucleotide
case object NucleotideC extends Nucleotide

object Nucleotide {
  def apply(c: Char) = c match {
    case 'T' => NucleotideT
    case 'A' => NucleotideA
    case 'G' => NucleotideG
    case 'C' => NucleotideC
  }
}
