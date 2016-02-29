package com.hamishdickson.helix.genomes

import scala.language.higherKinds

/**
  * Common DNA/RNA/mRNA things
  */
sealed trait Genome[+A]
case class GenomeString[A](g: List[A]) extends Genome[A] {

  /**
    * How many times does nucleotide n occur in genome g ?
    */
  def count[B](n: B): Int = g.foldRight(0)((h, t) => if (h == n) 1 + t else t)

  def hemmingDistance(t: GenomeString[A]): Int =
    (g zip t.g).foldRight(0){ case ((i,j), b) => if (i == j) b else 1 + b }
}