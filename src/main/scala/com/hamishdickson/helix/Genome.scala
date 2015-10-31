package com.hamishdickson.helix

import com.hamishdickson.helix.dna.Nucleotide

trait Genome[+A]

case class cons[A](nuc: Nucleotide) extends Genome[A]
case object None extends Genome[Nothing]