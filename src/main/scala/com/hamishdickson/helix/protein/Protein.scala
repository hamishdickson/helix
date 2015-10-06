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
  def proteinString(s: String): List[Protein] = s.toList.map(p => Protein(p))

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
}