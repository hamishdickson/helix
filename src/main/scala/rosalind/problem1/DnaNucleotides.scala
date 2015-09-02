package rosalind.problem1

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
class DnaNucleotides(nucleotides: String) {
  def count(nuc: Char) = nucleotides.count(_ == nuc)

  def count2(nuc: Char): Int = {
   def loop(c: List[Char]): Int = c match {
      case Nil => 0
      case x :: xs => if (x == nuc) 1 + loop(xs) else loop(xs)
    }

    loop(nucsAsList)
  }

  val nucsAsList: List[Char] = nucleotides.toList

  // maybe?
  def count3(nuc: Char): Int = nucsAsList.foldRight(0)((a, b) => if (a==nuc) 1 + b else b)
}
