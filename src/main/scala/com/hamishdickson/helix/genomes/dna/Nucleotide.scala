package com.hamishdickson.helix.genomes.dna

sealed trait Nucleotide {
  def asString = this match {
    case NucleotideA => "A"
    case NucleotideT => "T"
    case NucleotideG => "G"
    case NucleotideC => "C"
  }
}

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
