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

  def cgCounter(nuc: String): Double = nuc.count(_ == 'C') + nuc.count(_ == 'G')

  def hemmingDistanceCounter(nuc1: String, nuc2: String): Int = {
    def hdc(n1: String, n2: String, count: Int): Int = {
      if (n1.substring(0, 1) == n2.substring(0, 1)) {
        if (n1.length <= 1 || n2.length <= 1) return count
        hdc(n1.substring(1), n2.substring(1), count)
      }
      else {
        if (n1.length <= 1 || n2.length <= 1) return count + 1
        hdc(n1.substring(1), n2.substring(1), count + 1)
      }
    }

    hdc(nuc1, nuc2, 0)
  }

/*  def getProbOfTrait(k: Int, m: Int, n: Int): Double = {
    val tot = total

    ((k*k - k) + 2*(k*m) + 2*(k*n) + (.75*(m*m - m)) + 2*(.5*m*n))/tot


    def total: Int = (k + m + n)*(k + m + n -1)
  }*/
}
