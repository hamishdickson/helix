package com.hamishdickson.helix

trait Genome[+A]

case class Dna[+A](n: List[A]) extends Genome[A]
case class Rna[+A](n: List[A]) extends Genome[A]
case class MRna[+A](n: List[A]) extends Genome[A]