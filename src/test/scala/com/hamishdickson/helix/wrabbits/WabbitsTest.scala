package com.hamishdickson.helix.wrabbits

import org.scalatest.{Matchers, FlatSpec}

class WabbitsTest extends FlatSpec with Matchers {
  "After 5 months, a litter" should "reproduce 19 wabbits" in {
    val wabbits = new Wabbits()

    wabbits litter(5, 3) should be (19)
  }

  "Mortal wabbits" should "die after 2 months" in {
    val n: Int = 6
    val m: Int = 2

    val wabbits = new Wabbits

    wabbits.mortalLitter(3, m) should be (1)
    wabbits.mortalLitter(4, m) should be (1)
    wabbits.mortalLitter(5, m) should be (1)
    wabbits.mortalLitter(6, m) should be (1)
  }

  "Mortal wabbits" should "die after 3 months" in {
/*    val n: Int = 6
    val m: Int = 3

    val wabbits = new Wabbits

    wabbits.mortalLitter(3, m) should be (2)
    wabbits.mortalLitter(4, m) should be (2)
    wabbits.mortalLitter(5, m) should be (3)
    wabbits.mortalLitter(6, m) should be (4)
    wabbits.mortalLitter(7, m) should be (5)
    wabbits.mortalLitter(8, m) should be (7)*/

    pending
  }

  "Mortal wabbits" should "die after 4 months" in {
    val n: Int = 6
    val m: Int = 4

    val wabbits = new Wabbits

    wabbits.mortalLitter(3, m) should be (2)
    wabbits.mortalLitter(4, m) should be (3)
    wabbits.mortalLitter(5, m) should be (4)
    wabbits.mortalLitter(6, m) should be (6)
  }

  "Mortal wabbits" should "die after 5 months" in {
    val n: Int = 6
    val m: Int = 5

    val wabbits = new Wabbits

    wabbits.mortalLitter(3, m) should be (2)
    wabbits.mortalLitter(4, m) should be (3)
    wabbits.mortalLitter(5, m) should be (5)
    wabbits.mortalLitter(6, m) should be (7)
  }

  "Mortal wabbits" should "look immortal after 6 months" in {
    val n: Int = 6
    val m: Int = 6

    val wabbits = new Wabbits

    wabbits.mortalLitter(3, m) should be (2)
    wabbits.mortalLitter(4, m) should be (3)
    wabbits.mortalLitter(5, m) should be (5)
    wabbits.mortalLitter(6, m) should be (8)
  }
}
