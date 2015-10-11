package com.hamishdickson.helix.dna

import com.hamishdickson.helix.rna._

trait Dna

case class Genome(nucleotides: List[Nucleotide]) extends Dna {
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
  def count(nucleotide: Nucleotide): Int = nucleotides.foldRight(0)((a, b) => if (a == nucleotide) 1 + b else b)


  /**
   * An RNA string is a string formed from the alphabet containing 'A', 'C', 'G', and 'U'.
   *
   * Given a DNA string t corresponding to a coding strand, its transcribed RNA string u is formed by replacing all
   * occurrences of 'T' in t with 'U' in u.
   *
   * Given: A DNA string t having length at most 1000 nt.
   * Return: The transcribed RNA string of t.
   */
  def convert: Rna = RnaGenome(nucleotides map {
    case NucleotideA => RnaNucleotideA
    case NucleotideC => RnaNucleotideC
    case NucleotideG => RnaNucleotideG
    case NucleotideT => RnaNucleotideU
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
  def reverseComplementer: Genome = Genome(nucleotides.reverse map {
    case NucleotideA => NucleotideT
    case NucleotideT => NucleotideA
    case NucleotideC => NucleotideG
    case NucleotideG => NucleotideC
  })


  /**
   * The GC-content of a DNA string is given by the percentage of symbols in the string that are 'C' or 'G'. For example,
   * the GC-content of "AGCTATAG" is 37.5%. Note that the reverse complement of any DNA string has the same GC-content.
   *
   * DNA strings must be labeled when they are consolidated into a database. A commonly used method of string labeling is
   * called FASTA format. In this format, the string is introduced by a line that begins with '>', followed by some
   * labeling information. Subsequent lines contain the string itself; the first line to begin with '>' indicates the
   * label of the next string.
   *
   * In Rosalind's implementation, a string in FASTA format will be labeled by the ID "Rosalind_xxxx", where "xxxx"
   * denotes a four-digit code between 0000 and 9999.
   *
   * Given: At most 10 DNA strings in FASTA format (of length at most 1 kbp each).
   * Return: The ID of the string having the highest GC-content, followed by the GC-content of that string. Rosalind
   * allows for a default error of 0.001 in all decimal answers unless otherwise stated; please see the note on absolute
   * error below.
   */
  def cgCounter: Double = {
    val count: Double = nucleotides.foldRight(0)((a, b) => if (a == NucleotideC || a == NucleotideG) 1 + b else b)
    100 * count / nucleotides.length
  }


  /**
   * Given two strings s and t of equal length, the Hamming distance between s and t, denoted dH(s,t), is the number of
   * corresponding symbols that differ in s and t. See Figure 2.
   *
   * Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
   * Return: The Hamming distance dH(s,t).
   *
   * Implementation: l zip k gives you a list like (l1, k1), (l2, k2)... then foldRight to compare the tuple elements
   */
  def hemmingDistance(t: Genome): Int = {
    val z: List[(Nucleotide, Nucleotide)] = nucleotides zip t.nucleotides

    z.foldRight(0)((a, b) => if (a._1 == a._2) b else 1 + b)
  }

  /**
   * Given two strings s and t, t is a substring of s if t is contained as a contiguous collection of symbols in s
   * (as a result, t must be no longer than s).
   *
   * The position of a symbol in a string is the total number of symbols found to its left, including itself (e.g.,
   * the positions of all occurrences of 'U' in "AUGCUUCAGAAAGGUCUUACG" are 2, 5, 6, 15, 17, and 18). The symbol at
   * position i of s is denoted by s[i].
   *
   * A substring of s can be represented as s[j:k], where j and k represent the starting and ending positions of the
   * substring in s; for example, if s = "AUGCUUCAGAAAGGUCUUACG", then s[2:5] = "UGCU".
   *
   * The location of a substring s[j:k] is its beginning position j; note that t will have multiple locations in s if
   * it occurs more than once as a substring of s (see the Sample below).
   *
   * Given: Two DNA strings s and t (each of length at most 1 kbp).
   * Return: All locations of t as a substring of s.
   *
   * note: weirdly, there is an offset of 1 for the index position
   */
  def subSequencePositions(t: Genome): List[Int] = {
    def loop(u: Int, ts: List[Int]): List[Int] = {
      val w: Int = this.nucleotides.length - u
      val v: Genome = Genome(this.nucleotides.slice(u, t.nucleotides.length + u))

      if (w < t.nucleotides.length) ts
      else if (v.nucleotides == t.nucleotides) loop(u+1, (u+1) :: ts)
      else loop(u+1, ts)
    }

    if (t.nucleotides.isEmpty) List()
    else loop(0, List()).reverse
  }
}

object Dna {
  def apply(s: String): Genome = Genome(s.toList.map(g => Nucleotide(g)))

  def getProbOfTrait(k: Int, m: Int, n: Int): Double = {
    val tot: Int = (k + m + n)*(k + m + n -1)

    ((k*k - k) + 2*(k*m) + 2*(k*n) + (.75*(m*m - m)) + 2*(.5*m*n))/tot
  }

  def consensus(genomes: List[Genome]): Genome = {
    val a: List[List[Nucleotide]] = genomes.map(i => i.nucleotides).transpose

    val b: List[Nucleotide] = for {
      z <- meh(a)
    } yield biggest(z)

    Genome(b)
  }

  def meh(a: List[List[Nucleotide]]): List[(Int, Int, Int, Int)] = for {
    x <- a
  } yield (x.count(i => i == NucleotideA),
      x.count(i => i == NucleotideC),
      x.count(i => i == NucleotideG),
      x.count(i => i == NucleotideT))

  def formattedConsensus(genomes: List[Genome]): String = {
    val x: Genome = consensus(genomes)

    val y = meh(genomes.map(i => i.nucleotides).transpose)

    val a = y.foldRight(List[List[Int]]())((a,b) => a match { case (i,j,k,l) => List(i,j,k,l) :: b } ).transpose

    asString(x) +
      "\nA: " + a(0).mkString(" ") +
      "\nC: " + a(1).mkString(" ") +
      "\nG: " + a(2).mkString(" ") +
      "\nT: " + a(3).mkString(" ")
  }

  def asString(g: Genome): String = g.nucleotides.map(_.asString).mkString("")

  private def biggest(t: (Int, Int, Int, Int)): Nucleotide = {
    if (t._1 >= t._2 && t._1 >= t._3 && t._1 > t._4) NucleotideA
    else if (t._2 >= t._1 && t._2 >= t._3 && t._2 > t._4) NucleotideC
    else if (t._3 >= t._2 && t._3 >= t._1 && t._3 > t._4) NucleotideG
    else NucleotideT
  }
}
