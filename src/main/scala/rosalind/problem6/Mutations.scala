package rosalind.problem6

/**
 * Created by hamishdickson on 07/04/15.
 *
 * Given two strings s and t of equal length, the Hamming distance between s and t, denoted dH(s,t), is the number of
 * corresponding symbols that differ in s and t. See Figure 2.
 *
 * Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
 * Return: The Hamming distance dH(s,t).
 *
 */
class Mutations {
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
}
