package rosalind.problem3

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by hamishdickson on 31/03/15.
 */
class DnaBasePairsTest  extends FlatSpec with Matchers {
  "A reverse complementer" should "return the reverse complement of a nucleartide" in {
    var rc = new DnaBasePairs("AAAACCCGGT")

    rc.revComp should be ("ACCGGGTTTT")
  }
}
