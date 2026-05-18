package dataStructures

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

// https://www.hackerrank.com/challenges/dynamic-array/problem

object DynamicArray {

  def main(args: Array[String]): Unit = {

    val firstMultipleInput = StdIn.readLine.replaceAll("\\s+$", "").split(" ")
    val n = firstMultipleInput(0).toInt
    val q = firstMultipleInput(1).toInt
    val queries = Array.ofDim[Int](q, 3)
    for (i <- 0 until q) {
      queries(i) = StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)
    }
    val result = dynamicArray(2, queries)
    println(result.mkString("\n"))
  }

  def dynamicArray(n: Int, queries: Array[Array[Int]]): ArrayBuffer[Int] = {
    val arr: Array[ArrayBuffer[Int]] = Array.fill(n)(ArrayBuffer())
    var lastAnswer = 0
    val answers: ArrayBuffer[Int] = ArrayBuffer()

    queries.foreach { query =>
      if (query(0) == 1) {
        val idx = (query(1) ^ lastAnswer) % n
        arr(idx) += query(2)
      }
      if (query(0) == 2) {
        val idx = (query(1) ^ lastAnswer) % n
        lastAnswer = arr(idx)(query(2) % arr(idx).length)
        answers += lastAnswer
      }
    }

    answers
  }
}
