package rosalind.problem7

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by hamishdickson on 08/04/15.
 */
class Mendel1Test extends FlatSpec with Matchers {
  "Given any two from the given set of k, m, n organisims mating, the function" should "output the prob their offspring with be dominant in the trait" in {
    val mendel = new Mendel1

    val homozygousDominantPopulation = 2 // Yy
    val heterozygousDominantPopulation = 2 // yy
    val homozygousrecessivePopulation = 2 // er ...?

    mendel.getProbOfTrait(homozygousDominantPopulation, heterozygousDominantPopulation, homozygousrecessivePopulation) should be (0.78333)

    // from actual quiz

  }
}
