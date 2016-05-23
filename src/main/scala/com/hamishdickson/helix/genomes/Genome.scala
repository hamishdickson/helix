package com.hamishdickson.helix.genomes

import cats.Foldable

import scala.language.higherKinds

/**
  * Common DNA/RNA/mRNA things
  */
trait Genome[F[_]] extends Foldable[F] {
  /**
    * How many times does nucleotide n occur in genome g ?
    */
  def occurances[A](n: A): Int

  def hemmingDistance(t: Genome[F[_]]): Int
}