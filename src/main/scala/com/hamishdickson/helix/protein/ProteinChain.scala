package com.hamishdickson.helix.protein

/**
 * I don't know if chain is the right word here, but it works for me
 */
sealed trait ProteinRope

case class ProteinChain(s: List[Protein]) extends ProteinRope {
  override def toString: String = s.foldRight("")((a,b) => Protein.toChar(a) + b)
}

object ProteinRope {
  def apply(s: String):ProteinChain = ProteinChain(s.toList.map(p => Protein(p)))
}