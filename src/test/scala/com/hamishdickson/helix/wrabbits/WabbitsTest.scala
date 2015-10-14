package com.hamishdickson.helix.wrabbits

import org.scalatest.{Matchers, FlatSpec}

class WabbitsTest extends FlatSpec with Matchers {
  "After 5 months, a litter" should "reproduce 19 wabbits" in {
    val wabbits = new Wabbits()

    wabbits litter(5, 3) should be (19)
  }

  "Mortal wabbits" should "die after m months" in {
    val n: Int = 6
    val m: Int = 3

    val wabbits = new Wabbits

    wabbits.mortalLitter(3, m) should be (2)
    wabbits.mortalLitter(4, m) should be (2)
    wabbits.mortalLitter(5, m) should be (3)
  }
}
