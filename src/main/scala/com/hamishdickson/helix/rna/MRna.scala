package com.hamishdickson.helix.rna

import com.hamishdickson.helix.protein.{ProteinChain, Codon, Protein, Stop}

import scala.annotation.tailrec

sealed trait MRna

case class MRnaGenome(nucleotides: List[RnaNucleotide]) extends MRna {
  def toProteinList: ProteinChain = {
    @tailrec
    def loop(r: List[RnaNucleotide], ds: List[Protein]): List[Protein] = r match {
      case i :: j :: k :: tail => {
        val c: Codon = Codon(List(i, j, k))
        val p: Protein = c.toProtein

        if (p == Stop) ds
        else loop(tail, p :: ds)
      }
      case _ => ds
    }

    ProteinChain(loop(nucleotides, List()).reverse)
  }
}

object MRna {
  def apply(s: String): MRnaGenome = MRnaGenome(s.toList.map(g => RnaNucleotide(g)))
}
