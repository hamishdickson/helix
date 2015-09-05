package rosalind.Dna

/**
 * FIXME - I don't think Genome is the right word here, talk to a biologist at some point (or wikipedia...)
 *
 * This is identical to A Dna Genome, but with U instead of T
 */

sealed trait RnaGenome

case object RnaGenomeA extends RnaGenome
case object RnaGenomeG extends RnaGenome
case object RnaGenomeC extends RnaGenome
case object RnaGenomeU extends RnaGenome

object RnaGenome {
  def apply(c: Char) = c match {
    case 'A' => RnaGenomeA
    case 'G' => RnaGenomeG
    case 'C' => RnaGenomeC
    case 'U' => RnaGenomeU
  }
}
