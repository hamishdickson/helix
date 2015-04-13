package rosalind.problem3

/**
 * Created by hamishdickson on 31/03/15.
 *
 * In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'.
 *
 * The reverse complement of a DNA string s is the string sc formed by reversing the symbols of s, then taking the
 * complement of each symbol (e.g., the reverse complement of "GTCA" is "TGAC").
 *
 * Given: A DNA string s of length at most 1000 bp.
 * Return: The reverse complement sc of s.
 *
 */
class DnaBasePairs(s: String) {
  def revComp: String = s reverseMap {
    n => if (n.toUpper == 'A') 'T'
    else if (n.toUpper == 'T') 'A'
    else if (n.toUpper == 'C') 'G'
    else 'C'
  }
}

object test extends App {
  val s = "CCAGATC"

  val dna = new DnaBasePairs(s)

  println(dna.revComp)
}