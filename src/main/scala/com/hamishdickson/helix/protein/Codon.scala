package com.hamishdickson.helix.protein

/**
 * refactor - there has to be a better way to go about this..
 */

trait Codon

case object UUU extends Codon
case object UUC extends Codon
case object UUA extends Codon
case object UUG extends Codon
case object UCU extends Codon
case object UCC extends Codon
case object UCA extends Codon
case object UCG extends Codon
case object UAU extends Codon
case object UAC extends Codon
case object UAA extends Codon
case object UAG extends Codon
case object UGU extends Codon
case object UGC extends Codon
case object UGA extends Codon
case object UGG extends Codon
case object CUU extends Codon
case object CUC extends Codon
case object CUA extends Codon
case object CUG extends Codon
case object CCU extends Codon
case object CCC extends Codon
case object CCA extends Codon
case object CCG extends Codon
case object CAU extends Codon
case object CAC extends Codon
case object CAA extends Codon
case object CAG extends Codon
case object CGU extends Codon
case object CGC extends Codon
case object CGA extends Codon
case object CGG extends Codon
case object AUU extends Codon
case object AUC extends Codon
case object AUA extends Codon
case object AUG extends Codon
case object ACU extends Codon
case object ACC extends Codon
case object ACA extends Codon
case object ACG extends Codon
case object AAU extends Codon
case object AAC extends Codon
case object AAA extends Codon
case object AAG extends Codon
case object AGU extends Codon
case object AGC extends Codon
case object AGA extends Codon
case object AGG extends Codon
case object GUU extends Codon
case object GUC extends Codon
case object GUA extends Codon
case object GUG extends Codon
case object GCU extends Codon
case object GCC extends Codon
case object GCA extends Codon
case object GCG extends Codon
case object GAU extends Codon
case object GAC extends Codon
case object GAA extends Codon
case object GAG extends Codon
case object GGU extends Codon
case object GGC extends Codon
case object GGA extends Codon
case object GGG extends Codon

object Codon {
  def toProtein(c: Codon): Protein = c match {
    case UUU => ProteinF
    case UUC => ProteinF
    case UUA => ProteinL
    case UUG => ProteinL
    case UCU => ProteinS
    case UCC => ProteinS
    case UCA => ProteinS
    case UCG => ProteinS
    case UAU => ProteinY
    case UAC => ProteinY
    case UAA => Stop
    case UAG => Stop
    case UGU => ProteinC
    case UGC => ProteinC
    case UGA => Stop
    case UGG => ProteinW
    case CUU => ProteinL
    case CUC => ProteinL
    case CUA => ProteinL
    case CUG => ProteinL
    case CCU => ProteinP
    case CCC => ProteinP
    case CCA => ProteinP
    case CCG => ProteinP
    case CAU => ProteinH
    case CAC => ProteinH
    case CAA => ProteinQ
    case CAG => ProteinQ
    case CGU => ProteinR
    case CGC => ProteinR
    case CGA => ProteinR
    case CGG => ProteinR
    case AUU => ProteinI
    case AUC => ProteinI
    case AUA => ProteinI
    case AUG => ProteinM
    case ACU => ProteinT
    case ACC => ProteinT
    case ACA => ProteinT
    case ACG => ProteinT
    case AAU => ProteinN
    case AAC => ProteinN
    case AAA => ProteinK
    case AAG => ProteinK
    case AGU => ProteinS
    case AGC => ProteinS
    case AGA => ProteinR
    case AGG => ProteinR
    case GUU => ProteinV
    case GUC => ProteinV
    case GUA => ProteinV
    case GUG => ProteinV
    case GCU => ProteinA
    case GCC => ProteinA
    case GCA => ProteinA
    case GCG => ProteinA
    case GAU => ProteinD
    case GAC => ProteinD
    case GAA => ProteinE
    case GAG => ProteinE
    case GGU => ProteinG
    case GGC => ProteinG
    case GGA => ProteinG
    case GGG => ProteinG
  }
}
