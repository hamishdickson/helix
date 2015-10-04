package com.hamishdickson.helix

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
