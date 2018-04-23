package algorithms.implementation

// See https://www.hackerrank.com/challenges/magic-square-forming/problem for a description of this problem.

object MagicSquare {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val s = Array.ofDim[Int](3,3)
    println("Enter your square, one number at a time:")
    for(i <- 0 until 3) {
      for(j <- 0 until 3) {
        s(i)(j) = sc.nextInt()
      }
    }
    val result = formingMagicSquare(s)
    println(s"The minimum number of transformations needed to make your square magic is: $result")
  }

  def formingMagicSquare(s: Array[Array[Int]]): Int =  {
    val magicSquares = Vector(
      Array(Array(8, 1, 6), Array(3, 5, 7), Array(4, 9, 2)),
      Array(Array(6, 1, 8), Array(7, 5, 3), Array(2, 9, 4)),
      Array(Array(4, 9, 2), Array(3, 5, 7), Array(8, 1, 6)),
      Array(Array(2, 9, 4), Array(7, 5, 3), Array(6, 1, 8)),
      Array(Array(8, 3, 4), Array(1, 5, 9), Array(6, 7, 2)),
      Array(Array(4, 3, 8), Array(9, 5, 1), Array(2, 7, 6)),
      Array(Array(6, 7, 2), Array(1, 5, 9), Array(8, 3, 4)),
      Array(Array(2, 7, 6), Array(9, 5, 1), Array(4, 3, 8))
    )

    val result = magicSquares.map { magicRow =>
      val similarities = for(i <- 0 to 2; j <- 0 to 2) yield Math.abs(magicRow(i)(j) - s(i)(j))
      similarities.sum
    }
    result.min
  }
}
