package core

class DnaNucleotides {
  def count(nucleotides: String, nuc: Char) = nucleotides.count(_ == nuc)

  def convert(nucleotides: String): String = nucleotides.replace('T', 'U')

  def revComp(nucleotides: String): String = nucleotides reverseMap {
    n => if (n.toUpper == 'A') 'T'
    else if (n.toUpper == 'T') 'A'
    else if (n.toUpper == 'C') 'G'
    else 'C'
  }
}
