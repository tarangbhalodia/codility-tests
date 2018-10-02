package com.codility.tests

/*
* Write a function:

object Solution { def solution(a: Array[Int]): Int }

that, given an array A of N integers, returns the smallest positive integer (greater than 0) that does not occur in A.

For example, given A = [1, 3, 6, 4, 1, 2], the function should return 5.

Given A = [1, 2, 3], the function should return 4.

Given A = [−1, −3], the function should return 1.

Write an efficient algorithm for the following assumptions:

N is an integer within the range [1..100,000];
each element of array A is an integer within the range [−1,000,000..1,000,000].*/

object MissingInteger extends App {

  println(solution(Array(2, 4, 5, 6)))

  def solution(a: Array[Int]): Int = {
    val arrayWithPostives = a.filter(_ > 0)
    if (arrayWithPostives.isEmpty) return 1

    val sortedArray = arrayWithPostives.sorted
    if (sortedArray.head > 1) return 1

    for (i <- 0 until sortedArray.length - 1) {
      if ((sortedArray(i + 1) - sortedArray(i)) > 1) {
        return sortedArray(i) + 1
      }
    }
    sortedArray.last + 1
  }

}
