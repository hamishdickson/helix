package rosalind.Dna

/**
 * FIXME - I don't think Genome is the right word here, talk to a biologist at some point (or wikipedia...)
 *
 * Note: I wonder if genome should be a trait, with case classes, one for each of the 4 genomes - there are only
 * 4 after all........ think about this
 *
 *
 * Note 2: OK, Implemented... this may or may not be a terrible idea...
 */
sealed trait Genome

case object GenomeT extends Genome
case object GenomeA extends Genome
case object GenomeG extends Genome
case object GenomeC extends Genome

object Genome {
  def apply(c: Char) = c match {
    case 'T' => GenomeT
    case 'A' => GenomeA
    case 'G' => GenomeG
    case 'C' => GenomeC
  }
}
