package com.hamishdickson.helix.wrabbits

import scala.annotation.tailrec

/**
 * A sequence is an ordered collection of objects (usually numbers), which are allowed to repeat. Sequences can be
 * finite or infinite. Two examples are the finite sequence (π,−2‾√,0,π) and the infinite sequence of odd numbers
 * (1,3,5,7,9,…). We use the notation an to represent the n-th term of a sequence.
 *
 * A recurrence relation is a way of defining the terms of a sequence with respect to the values of previous terms. In
 * the case of Fibonacci's rabbits from the introduction, any given month will contain the rabbits that were alive the
 * previous month, plus any new offspring. A key observation is that the number of offspring in any month is equal to
 * the number of rabbits that were alive two months prior. As a result, if Fn represents the number of rabbit pairs
 * alive after the n-th month, then we obtain the Fibonacci sequence having terms Fn that are defined by the recurrence
 * relation Fn=Fn−1+Fn−2 (with F1=F2=1 to initiate the sequence). Although the sequence bears Fibonacci's name, it was
 * known to Indian mathematicians over two millennia ago.
 *
 * When finding the n-th term of a sequence defined by a recurrence relation, we can simply use the recurrence relation
 * to generate terms for progressively larger values of n. This problem introduces us to the computational technique of
 * dynamic programming, which successively builds up solutions by using the answers to smaller cases.
 *
 *
 * Given: Positive integers n≤40 and k≤5.
 * Return: The total number of rabbit pairs that will be present after n months if we begin with 1 pair and in each
 * generation, every pair of reproduction-age rabbits produces a litter of k rabbit pairs (instead of only 1 pair).
 *
 */
class Wabbits {
  def litter(months: BigInt, litterSize: BigInt): BigInt = {

    // fluffy tail recursive
    @tailrec
    def lit(count: BigInt, a: BigInt, b: BigInt): BigInt = {
      if (count <= 0) a
      else lit(count - 1, b, litterSize*a + b)
    }

    lit(months, 0, 1)
  }

  /**
   * Recall the definition of the Fibonacci numbers from “Rabbits and Recurrence Relations”, which followed the
   * recurrence relation Fn=Fn−1+Fn−2 and assumed that each pair of rabbits reaches maturity in one month and produces
   * a single pair of offspring (one male, one female) each subsequent month.
   *
   * Our aim is to somehow modify this recurrence relation to achieve a dynamic programming solution in the case that
   * all rabbits die out after a fixed number of months. See Figure 4 for a depiction of a rabbit tree in which rabbits
   * live for three months (meaning that they reproduce only twice before dying).
   *
   * Given: Positive integers n≤100 and m≤20.
   * Return: The total number of pairs of rabbits that will remain after the n-th month if all rabbits live for m months
   *
   * Fn = Fn=Fn−1 + Fn−2 - Fn-m
   * 1 1 2 3 5 8
   * 1 1 2 2 3 4
   */
  @deprecated("Do not use - suspected bug in here")
  def mortalLitter(n: Int, m: Int): BigInt = {
    val j: List[BigInt] = List(1, 1)

    def loop(q: Int, lx: List[BigInt]): List[BigInt] = {
      def k(p: BigInt, l: List[BigInt]): List[BigInt] = {
        if (m - 1 < l.size)
          // this is wrong - should ne n - m - 1, not m - 1
          l.head + l(1) - l(m - 1) :: l
        else
          l.head + l(1) :: l
      }
      if (q == n) lx
      else loop(q + 1, k(q, lx))
    }

    loop(2, j).head
  }
}
