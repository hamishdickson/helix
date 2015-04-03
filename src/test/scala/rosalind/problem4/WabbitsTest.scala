package rosalind.problem4

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by hamishdickson on 02/04/15.
 *
 */
class WabbitsTest extends FlatSpec with Matchers {
  "After 5 months, a litter" should "reproduce 19 wabbits" in {
    val wabbits = new Wabbits()

    wabbits litter(5, 3) should be (19)
  }
}