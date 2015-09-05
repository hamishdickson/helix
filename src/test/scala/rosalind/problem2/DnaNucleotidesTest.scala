package rosalind.problem2

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by hamishdickson on 30/03/15.
 *
 * depreciated
 */
class DnaNucleotidesTest extends FlatSpec with Matchers {
  "An rna converter" should "be able to convert dna to rna" in {
    val dnaString: String = "GATGGAACTTGACTACGTAAATT"

    val converter: DnaNucleotides = new DnaNucleotides(dnaString)

    converter.convert should be ("GAUGGAACUUGACUACGUAAAUU")
  }
}
