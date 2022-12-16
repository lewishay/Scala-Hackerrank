package algorithms.implementation

import scala.annotation.tailrec

// See https://www.hackerrank.com/challenges/almost-sorted/problem for a description of this problem

object AlmostSorted {

  def main(args: Array[String]): Unit = {
    val stdin = scala.io.StdIn
    println("Please enter a sequence of numbers, separated by spaces.")
    val seq = stdin.readLine.split(" ").map(_.toInt)
    print(almostSorted(seq.toVector))
  }

  @tailrec
  def isSorted(vector: Vector[Int], previousNum: Int = 0): Boolean = {
    if(vector.isEmpty) true else {
      if(vector.head >= previousNum) isSorted(vector.tail, vector.head) else false
    }
  }

  def swapElem(vector: Vector[Int], leftIndex: Int, rightIndex: Int): Vector[Int] =
    vector.patch(leftIndex, Vector(vector(rightIndex)), 1).patch(rightIndex, Vector(vector(leftIndex)), 1)

  def reverseSection(vector: Vector[Int], leftIndex: Int, rightIndex: Int): Vector[Int] = {
    val sectionToReverse = vector.slice(leftIndex, rightIndex + 1).reverse
    vector.take(leftIndex) ++ sectionToReverse ++ vector.drop(rightIndex + 1)
  }

  @tailrec
  def getIndex(vector: Vector[Int], func: (Int, Int) => Boolean, indexCount: Int = 0): Option[Int] = vector match {
    case Vector(int1, int2) =>
      if(func(int2, int1)) Some(indexCount) else None
    case Vector(int1, int2, _*) if func(int2, int1) => Some(indexCount)
    case _ => getIndex(vector.tail, func, indexCount + 1)
  }

  def almostSorted(vector: Vector[Int]): String = {
    if(isSorted(vector)) "yes" else {
      val leftIndex = getIndex(vector, _ < _)
      val rightIndex = getIndex(vector.reverse, _ > _).map(vector.length - _ - 1)
      (leftIndex, rightIndex) match {
        case (Some(lIndex), Some(rIndex)) =>
          if(isSorted(swapElem(vector, lIndex, rIndex))) {
            s"yes\nswap ${lIndex + 1} ${rIndex + 1}"
          } else if(isSorted(reverseSection(vector, lIndex, rIndex))) {
            s"yes\nreverse ${lIndex + 1} ${rIndex + 1}"
          } else {
            "no"
          }
        case _ => "no"
      }
    }
  }
}
