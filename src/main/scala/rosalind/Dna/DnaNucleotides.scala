package rosalind.Dna

/**
 * Again, probably needs renaming
 */

trait Dna

case class DnaNucleotides(nucleotides: List[Genome]) extends Dna {
  /**
   * A string is simply an ordered collection of symbols selected from some alphabet and formed into a word; the length
   * of a string is the number of symbols that it contains.
   *
   * An example of a length 21 DNA string (whose alphabet contains the symbols 'A', 'C', 'G', and 'T') is "ATGCTTCAGAAAGGTCTTACG."
   *
   * Create a function with the following attributes:
   * takes:  A DNA string s of length at most 1000 nt.
   * returns: Four integers (separated by spaces) counting the respective number of times that the symbols 'A', 'C', 'G', and 'T' occur in s.
   */
  def count(genome: Genome): Int = nucleotides.foldRight(0)((a, b) => if (a == genome) 1 + b else b)

  /**
   * An RNA string is a string formed from the alphabet containing 'A', 'C', 'G', and 'U'.
   *
   * Given a DNA string t corresponding to a coding strand, its transcribed RNA string u is formed by replacing all
   * occurrences of 'T' in t with 'U' in u.
   *
   * Given: A DNA string t having length at most 1000 nt.
   * Return: The transcribed RNA string of t.
   */
  def convert: Rna = RnaNucleotides(nucleotides.map {
    case GenomeA => RnaGenomeA
    case GenomeC => RnaGenomeC
    case GenomeG => RnaGenomeG
    case GenomeT => RnaGenomeU
  })

  /**
   * In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'.
   *
   * The reverse complement of a DNA string s is the string sc formed by reversing the symbols of s, then taking the
   * complement of each symbol (e.g., the reverse complement of "GTCA" is "TGAC").
   *
   * Given: A DNA string s of length at most 1000 bp.
   * Return: The reverse complement sc of s.
   */
  def reverseComplementer: DnaNucleotides = new DnaNucleotides(nucleotides.reverse map {
    case GenomeA => GenomeT
    case GenomeT => GenomeA
    case GenomeC => GenomeG
    case _ => GenomeC
  })
}

object Dna {
  def apply(s: String): DnaNucleotides = DnaNucleotides(s.toList.map(g => Genome(g)))
}
