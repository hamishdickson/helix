package com.hamishdickson.helix.dna

import com.hamishdickson.helix.protein.{Stop, Codon, Protein}
import com.hamishdickson.helix.rna.RnaNucleotide

trait MRna

case class MRnaGenome(nucleotides: List[RnaNucleotide]) extends MRna {
  def toProteinList: List[Protein] = {
    def loop(r: List[RnaNucleotide], ds: List[Protein]): List[Protein] = r match {
      case i :: j :: k :: tail => {
        val c: Codon = Codon.mRnaToCodon(List(i, j, k))
        val p: Protein = Codon.toProtein(c)

        if (p == Stop) ds
        else loop(tail, p :: ds)
      }
      case _ => ds
    }

    loop(nucleotides, List()).reverse
  }
}

object MRna {
  def apply(s: String): MRnaGenome = MRnaGenome(s.toList.map(g => RnaNucleotide(g)))
}
