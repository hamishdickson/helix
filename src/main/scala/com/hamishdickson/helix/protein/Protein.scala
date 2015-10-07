package com.hamishdickson.helix.protein

sealed trait Protein

case object ProteinA extends Protein
case object ProteinC extends Protein
case object ProteinD extends Protein
case object ProteinE extends Protein
case object ProteinF extends Protein
case object ProteinG extends Protein
case object ProteinH extends Protein
case object ProteinI extends Protein
case object ProteinK extends Protein
case object ProteinL extends Protein
case object ProteinM extends Protein
case object ProteinN extends Protein
case object ProteinP extends Protein
case object ProteinQ extends Protein
case object ProteinR extends Protein
case object ProteinS extends Protein
case object ProteinT extends Protein
case object ProteinV extends Protein
case object ProteinW extends Protein
case object ProteinY extends Protein
case object Stop extends Protein

object Protein {
  def apply(c: Char) = c match {
    case 'A' => ProteinA
    case 'C' => ProteinC
    case 'D' => ProteinD
    case 'E' => ProteinE
    case 'F' => ProteinF
    case 'G' => ProteinG
    case 'H' => ProteinH
    case 'I' => ProteinI
    case 'K' => ProteinK
    case 'L' => ProteinL
    case 'M' => ProteinM
    case 'N' => ProteinN
    case 'P' => ProteinP
    case 'Q' => ProteinQ
    case 'R' => ProteinR
    case 'S' => ProteinS
    case 'T' => ProteinT
    case 'V' => ProteinV
    case 'W' => ProteinW
    case 'Y' => ProteinY
  }

  def toChar(p: Protein): Char = p match {
    case ProteinA => 'A'
    case ProteinC => 'C'
    case ProteinD => 'D'
    case ProteinE => 'E'
    case ProteinF => 'F'
    case ProteinG => 'G'
    case ProteinH => 'H'
    case ProteinI => 'I'
    case ProteinK => 'K'
    case ProteinL => 'L'
    case ProteinM => 'M'
    case ProteinN => 'N'
    case ProteinP => 'P'
    case ProteinQ => 'Q'
    case ProteinR => 'R'
    case ProteinS => 'S'
    case ProteinT => 'T'
    case ProteinV => 'V'
    case ProteinW => 'W'
    case _ => 'Y'
  }
}