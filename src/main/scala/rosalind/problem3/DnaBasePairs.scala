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
  def revComp: String = {
    val u = s.reverse.toCharArray

    var res = ""

    for (t <- u) t match {
      case 'A' => res = res + "T"
      case 'T' => res = res + "A"
      case 'C' => res = res + "G"
      case 'G' => res = res + "C"
    }

    res
  }
}
