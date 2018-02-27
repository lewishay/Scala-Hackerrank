package algorithms.implementation

// See https://www.hackerrank.com/challenges/the-grid-search/problem for a description of this problem.

object GridSearch {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    println("Enter number of grid rows:")
    val gridRows = sc.nextInt()
    println("Enter number of grid columns:")
    val gridCols = sc.nextInt()
    println("Enter the contents of each grid row:")
    val grid = new Array[String](gridRows)
    for(i <- 0 until gridRows) {
      val nextInput = sc.next()
      val nextLine: String = nextInput.length match {
        case x if x > gridCols => nextInput.substring(0, gridCols)
        case x if x < gridCols => nextInput.padTo(gridCols, '#')
        case _ => nextInput
      }
      grid(i) = nextLine
    }
    println("Enter number of pattern rows:")
    val patternRows = sc.nextInt()
    println("Enter number of pattern columns:")
    val patternCols = sc.nextInt()
    println("Enter the contents of each pattern row:")
    val pattern = new Array[String](patternRows)
    for(i <- 0 until patternRows) {
      val nextInput = sc.next()
      val nextLine: String = nextInput.length match {
        case x if x > gridCols => nextInput.substring(0, patternCols)
        case x if x < gridCols => nextInput.padTo(patternCols, '#')
        case _ => nextInput
      }
      pattern(i) = nextLine
    }
    val result = gridSearch(grid, pattern)
    println(result)
  }

  def gridSearch(grid: Array[String], pattern: Array[String]): String = grid match {
    case _ if grid.length < pattern.length => "NO! The pattern is not in the grid!"
    case _ =>
      if(grid.head.contains(pattern.head) && patternCheck(grid.take(pattern.length), pattern)) {
        "YES! The pattern is in the grid!"
      } else {
        gridSearch(grid.tail, pattern)
      }
  }

  def patternCheck(gridSection: Array[String], pattern: Array[String]): Boolean = {
    val indices = pattern.indices.map(x => getAllIndexes(gridSection(x), pattern(x)))
    if(indices.reduce(_.intersect(_)).nonEmpty) {
      pattern.indices.forall { x =>
        gridSection(x).contains(pattern(x))
      }
    } else false
  }

  def getAllIndexes(source: String, target: String, index: Int = 0): Array[Int] = {
    val targetIndex = source.indexOf(target, index)
    if(targetIndex != -1) {
      Array(targetIndex) ++ getAllIndexes(source, target, targetIndex + 1)
    } else {
      Array()
    }
  }
}
