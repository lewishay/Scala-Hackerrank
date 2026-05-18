package dataStructures

import scala.io.StdIn

// https://www.hackerrank.com/challenges/array-left-rotation/problem

object RotateLeft {

    def main(args: Array[String]): Unit = {
      val firstMultipleInput = StdIn.readLine().replaceAll("\\s+$", "").split(" ")
      val n = firstMultipleInput(0).toInt
      val d = firstMultipleInput(1).toInt
      val arr = StdIn.readLine().replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)
      val result = rotateLeft(d, arr)
      println(result.mkString(" "))
    }

    def rotateLeft(d: Int, arr: Array[Int]): Array[Int] = {
      if (d == arr.length) arr
      else arr.drop(d) ++ arr.take(d)
    }
}
