package com.github.arturopala.minmaxmissing

class ChalkBoard(minInc: Int, maxExc: Int) {

  import ChalkBoard._

  require(minInc <= maxExc, "minInc has to be less or equal to maxExc")

  private val length = ((maxExc - minInc) / 64) + 1
  private val board = new Array[Long](length)

  board(length - 1) = mask.drop((maxExc - minInc) % 64).reduce(_ | _)

  def mark(number: Int): this.type = {
    if (number >= minInc && number < maxExc) {
      val i = (number - minInc) / 64
      val j = (number - minInc) % 64
      board(i) = board(i) | mask(j)
    }
    this
  }

  def has(number: Int): Boolean =
    if (number >= minInc && number < maxExc) {
      val i = (number - minInc) / 64
      val j = (number - minInc) % 64
      (board(i) & mask(j)) != 0
    } else false

  def minMissing: Option[Int] = {
    var i = 0
    while (i < length) {
      if (board(i) != full) {
        val value = minInc + i * 64 + minOf(~board(i))
        return Some(value)
      } else i = i + 1
    }
    None
  }

  def maxMissing: Option[Int] = {
    var i = length - 1
    while (i >= 0) {
      if (board(i) != full) {
        val value = minInc + i * 64 + maxOf(~board(i))
        return Some(value)
      } else i = i - 1
    }
    None
  }

  private def minOf(l: Long): Int = {
    var j = 0
    while (j < 64) {
      if ((l & mask(j)) != 0) {
        return j
      } else j = j + 1
    }
    j
  }

  private def maxOf(l: Long): Int = {
    var j = 64 - 1
    while (j >= 0) {
      if ((l & mask(j)) != 0) {
        return j
      } else j = j - 1
    }
    j
  }

}

object ChalkBoard {
  private val mask = new Array[Long](64).zipWithIndex.map {
    case (_, j) => 1L << j
  }
  private val full: Long = mask.reduce(_ | _)
}
