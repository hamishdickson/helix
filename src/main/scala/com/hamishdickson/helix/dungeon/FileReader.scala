package com.hamishdickson.helix.dungeon

import scala.io.Source

/**
 * Reads a file and returns a string - useful when you have a small genome in a file
 */
object FileReader {
  def getStringFromFile(f: String): String = {
    Source.fromFile(f).getLines.mkString
  }
}
