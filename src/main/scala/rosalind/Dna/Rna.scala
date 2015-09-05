package rosalind.Dna

/**
 * This probably needs renaming after I spend a day on wikipedia
 */

trait Rna

case class RnaNucleotides(nucleotides: List[RnaGenome]) extends Rna

object Rna {
  def apply(s: String): RnaNucleotides = RnaNucleotides(s.toList.map(g => RnaGenome(g)))
}