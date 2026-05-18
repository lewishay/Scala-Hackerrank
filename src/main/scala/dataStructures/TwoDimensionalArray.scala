package dataStructures

import scala.io.StdIn

// https://www.hackerrank.com/challenges/2d-array/problem

object TwoDimensionalArray {

  def main(args: Array[String]): Unit = {
    val arr = Array.ofDim[Int](6, 6)
    for (i <- 0 until 6) {
      arr(i) = StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)
    }
    val result = hourglassSum(arr)
    println(result)
  }

  def hourglassSum(arr: Array[Array[Int]]): Int = {
    val hourglassSums = Seq(
      arr(0)(0) + arr(0)(1) + arr(0)(2) + arr(1)(1) + arr(2)(0) + arr(2)(1) + arr(2)(2),
      arr(0)(1) + arr(0)(2) + arr(0)(3) + arr(1)(2) + arr(2)(1) + arr(2)(2) + arr(2)(3),
      arr(0)(2) + arr(0)(3) + arr(0)(4) + arr(1)(3) + arr(2)(2) + arr(2)(3) + arr(2)(4),
      arr(0)(3) + arr(0)(4) + arr(0)(5) + arr(1)(4) + arr(2)(3) + arr(2)(4) + arr(2)(5),

      arr(1)(0) + arr(1)(1) + arr(1)(2) + arr(2)(1) + arr(3)(0) + arr(3)(1) + arr(3)(2),
      arr(1)(1) + arr(1)(2) + arr(1)(3) + arr(2)(2) + arr(3)(1) + arr(3)(2) + arr(3)(3),
      arr(1)(2) + arr(1)(3) + arr(1)(4) + arr(2)(3) + arr(3)(2) + arr(3)(3) + arr(3)(4),
      arr(1)(3) + arr(1)(4) + arr(1)(5) + arr(2)(4) + arr(3)(3) + arr(3)(4) + arr(3)(5),

      arr(2)(0) + arr(2)(1) + arr(2)(2) + arr(3)(1) + arr(4)(0) + arr(4)(1) + arr(4)(2),
      arr(2)(1) + arr(2)(2) + arr(2)(3) + arr(3)(2) + arr(4)(1) + arr(4)(2) + arr(4)(3),
      arr(2)(2) + arr(2)(3) + arr(2)(4) + arr(3)(3) + arr(4)(2) + arr(4)(3) + arr(4)(4),
      arr(2)(3) + arr(2)(4) + arr(2)(5) + arr(3)(4) + arr(4)(3) + arr(4)(4) + arr(4)(5),

      arr(3)(0) + arr(3)(1) + arr(3)(2) + arr(4)(1) + arr(5)(0) + arr(5)(1) + arr(5)(2),
      arr(3)(1) + arr(3)(2) + arr(3)(3) + arr(4)(2) + arr(5)(1) + arr(5)(2) + arr(5)(3),
      arr(3)(2) + arr(3)(3) + arr(3)(4) + arr(4)(3) + arr(5)(2) + arr(5)(3) + arr(5)(4),
      arr(3)(3) + arr(3)(4) + arr(3)(5) + arr(4)(4) + arr(5)(3) + arr(5)(4) + arr(5)(5),
    )

    hourglassSums.max
  }
}
