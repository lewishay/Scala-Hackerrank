package algorithms.implementation

// See https://www.hackerrank.com/challenges/larrys-array/problem for a description of this problem.

object LarrysArray {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    println("Please enter the number of elements in the array:")
    val n = sc.nextInt()
    println("Please enter each number one-by-one:")
    val arr = new Array[Int](n)
    for(i <- 0 until n) {
      arr(i) = sc.nextInt()
    }
    val result = larrysArray(arr)
    println(s"Can a perfectly sorted array be made? $result")
  }

  def larrysArray(a: Array[Int]): String = {
    val sorted = a.sorted // the Array sort is outside of the recursive function to aid performance
    def recFunc(arr: Array[Int], currentNum: Int = 1): String = arr match {
      case _ if arr.sameElements(sorted.drop(currentNum - 1)) => "YES"
      case _ if arr.head == currentNum => recFunc(arr.tail, currentNum + 1)
      case _ if arr.length < 3 => "NO"
      case _ =>
        val index = arr.indexOf(currentNum)
        val safety = if (index == 1) 1 else 0
        val rotatedNums =
          rotate(arr(index - 2 + safety), arr(index - 1 + safety), arr(index + safety))
        val updatedArray = arr.clone
        updatedArray(index - 2 + safety) = rotatedNums._1
        updatedArray(index - 1 + safety) = rotatedNums._2
        updatedArray(index + safety) = rotatedNums._3
        recFunc(updatedArray, currentNum)
    }
    recFunc(a)
  }

  def rotate(a: Int, b: Int, c: Int): (Int, Int, Int) = {
    val smallest = Math.min(Math.min(a, b), c)
    (b == smallest, c == smallest) match {
      case (true, false) => (b, c, a)
      case (false, true) => (c, a, b)
      case _ => (a, b, c)
    }
  }
}
