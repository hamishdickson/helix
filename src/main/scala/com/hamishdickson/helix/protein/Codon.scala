package com.hamishdickson.helix.protein

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
  def toProteinList(s: String): List[Protein] = {
    def loop(d: String, ds: List[Protein]): List[Protein] = {
      if (d.length < 3) ds
      else {
        val f: String = d.substring(0, 3)
        val g: Codon = Codon(f.toList)
        val h: Protein = Codon.toProtein(g)

        if (h == Stop) ds
        else loop(d.substring(3), h :: ds)
      }
    }

    loop(s, List()).reverse
  }

  def apply(cx: List[Char]) = cx match {
    case List('U','U','U') => UUU
    case List('U','U','C') => UUC
    case List('U','U','A') => UUA
    case List('U','U','G') => UUG
    case List('U','C','U') => UCU
    case List('U','C','C') => UCC
    case List('U','C','A') => UCA
    case List('U','C','G') => UCG
    case List('U','A','U') => UAU
    case List('U','A','C') => UAC
    case List('U','A','A') => UAA
    case List('U','A','G') => UAG
    case List('U','G','U') => UGU
    case List('U','G','C') => UGC
    case List('U','G','A') => UGA
    case List('U','G','G') => UGG
    case List('C','U','U') => CUU
    case List('C','U','C') => CUC
    case List('C','U','A') => CUA
    case List('C','U','G') => CUG
    case List('C','C','U') => CCU
    case List('C','C','C') => CCC
    case List('C','C','A') => CCA
    case List('C','C','G') => CCG
    case List('C','A','U') => CAU
    case List('C','A','C') => CAC
    case List('C','A','A') => CAA
    case List('C','A','G') => CAG
    case List('C','G','U') => CGU
    case List('C','G','C') => CGC
    case List('C','G','A') => CGA
    case List('C','G','G') => CGG
    case List('A','U','U') => AUU
    case List('A','U','C') => AUC
    case List('A','U','A') => AUA
    case List('A','U','G') => AUG
    case List('A','C','U') => ACU
    case List('A','C','C') => ACC
    case List('A','C','A') => ACA
    case List('A','C','G') => ACG
    case List('A','A','U') => AAU
    case List('A','A','C') => AAC
    case List('A','A','A') => AAA
    case List('A','A','G') => AAG
    case List('A','G','U') => AGU
    case List('A','G','C') => AGC
    case List('A','G','A') => AGA
    case List('A','G','G') => AGG
    case List('G','U','U') => GUU
    case List('G','U','C') => GUC
    case List('G','U','A') => GUA
    case List('G','U','G') => GUG
    case List('G','C','U') => GCU
    case List('G','C','C') => GCC
    case List('G','C','A') => GCA
    case List('G','C','G') => GCG
    case List('G','A','U') => GAU
    case List('G','A','C') => GAC
    case List('G','A','A') => GAA
    case List('G','A','G') => GAG
    case List('G','G','U') => GGU
    case List('G','G','C') => GGC
    case List('G','G','A') => GGA
    case List('G','G','G') => GGG
  }

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
