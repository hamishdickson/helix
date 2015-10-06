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
}

object Dna {
  def apply(s: String): Genome = Genome(s.toList.map(g => Nucleotide(g)))

  def getProbOfTrait(k: Int, m: Int, n: Int): Double = {
    val tot: Int = (k + m + n)*(k + m + n -1)

    ((k*k - k) + 2*(k*m) + 2*(k*n) + (.75*(m*m - m)) + 2*(.5*m*n))/tot
  }
}