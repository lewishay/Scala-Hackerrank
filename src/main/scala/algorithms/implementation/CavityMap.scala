package algorithms.implementation

// See https://www.hackerrank.com/challenges/cavity-map/problem for a description of this problem.

object CavityMap {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    println("Please enter the size of the grid:")
    val n = sc.nextInt()
    println("Please enter the rows of the grid, one line at a time:") //assumes the user complies with the size constraints
    val grid = new Array[String](n)
    for(grid_i <- 0 until n) {
      grid(grid_i) = sc.next()
    }
    val result = cavityMap(grid)
    println("Result:")
    println(result.mkString("\n"))
  }

  def cavityMap(grid: Array[String]): IndexedSeq[String] = {
    if(grid.length < 3) grid else {
      for(rowNum <- grid.indices) yield computeRow(grid, rowNum)
    }
  }

  def computeRow(grid: Array[String], rowNum: Int): String = rowNum match {
    case 0 => grid.head
    case _ if rowNum == (grid.length - 1) => grid.last
    case _ => cavity(grid, grid(rowNum), rowNum, 0)
  }

  def cavity(grid: Array[String], row: String, rowNum: Int, index: Int): String = index match {
    case 0 => row(index) + cavity(grid, row, rowNum, index + 1)
    case _ if index == (row.length - 1) => row(index).toString
    case _ if
    row(index) > row(index - 1) &&
      row(index) > row(index + 1) &&
      row(index) > grid(rowNum - 1).charAt(index) &&
      row(index) > grid(rowNum + 1).charAt(index) =>
      "X" + cavity(grid, row, rowNum, index + 1)
    case _ => row(index) + cavity(grid, row, rowNum, index + 1)
  }
}
