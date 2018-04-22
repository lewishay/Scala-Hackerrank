def formingMagicSquare(s: Array[Array[Int]]): Int =  {
  (0 to 2).map(i => formMagic(i, s(i))).sum
}

def formMagic(column: Int, input: Array[Int]): Int = {
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

  val relevantTrios: Seq[Array[Int]] = for(i <- 0 to 7) yield magicSquares(i)(column)
  relevantTrios.map { magicTrio =>
    (0 to 2).map(i => Math.abs(magicTrio(i) - input(i))).sum
  }
  relevantTrios.flatten.min
}

//formingMagicSquare(Array(Array(8, 2, 2), Array(7, 5, 2), Array(2, 9, 2)))
formMagic(0, Array(8, 1, 8))