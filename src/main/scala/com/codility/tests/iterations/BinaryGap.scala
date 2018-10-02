package com.codility.tests.iterations

object BinaryGap extends App {

  println(solution(32))

  def solution(n: Int): Int = {
    val binaryStringArray = n.toBinaryString.toCharArray

    if (binaryStringArray.length < 3) return 0

    val indicesOf1 = binaryStringArray.zipWithIndex.filter(_._1 == '1').map(_._2).toList
    if (indicesOf1.length < 2) return 0
    val binaryGaps = indicesOf1.sliding(2).map { case Seq(x, y, _*) => y - x }
    if (binaryGaps.isEmpty) 0
    else binaryGaps.max - 1
  }
}
