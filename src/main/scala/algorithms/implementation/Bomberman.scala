package algorithms.implementation

import scala.annotation.tailrec

// See https://www.hackerrank.com/challenges/bomber-man/problem for a description of this problem.

object Bomberman {
  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    // Row and column size as well as the grid contents have been set to static values here to avoid input inconsistencies
    val r = 10
    val c = 10
    val grid = Array(
      "...O.OO...",
      ".OO......O",
      "OO...O....",
      "..........",
      "O..O..O..O",
      "OOOO..O.OO",
      "....O..O..",
      "......O...",
      "..O......O",
      "....OOOO.."
    )
    println("Enter number of seconds passed:")
    val n = sc.nextInt()
    val result = bomberMan(r, c, n, grid)
    println(s"After $n seconds, the grid looks like this:\n${result.mkString("\n")}")
  }

  def bomberMan(r: Int, c: Int, n: Int, grid: Array[String]): Array[String] = n match {
    case 1 => grid
    case _ if n % 2 == 0 => Array.fill(r)("O" * c)
    case _ =>
      val newGrid = gridIteration(grid)
      if((n - 1) % 4 == 0) gridIteration(newGrid) else newGrid
  }

  def gridIteration(grid: Array[String]): Array[String] = {
    val result = vertBombUpdate(grid.map(horizBombUpdate))
    result.map(_.replace('O', 'X').replace('.', 'O').replace('X', '.'))
  }

  @tailrec
  def vertBombUpdate(grid: Array[String], rowNums: (Int, Int) = (0, 1)): Array[String] = rowNums match {
    case _ if rowNums._2 == grid.length => grid
    case _ =>
      val topRowIndexes = if(grid(rowNums._1).contains('O')) getIndexes(grid(rowNums._1)) else List()
      val bottomRowIndexes =  if(grid(rowNums._2).contains('O')) getIndexes(grid(rowNums._2)) else List()
      val validIndexes =
        topRowIndexes.filter(!bottomRowIndexes.contains(_)) ::: bottomRowIndexes.filter(!topRowIndexes.contains(_))
      val updatedTopRow = grid(rowNums._1).zipWithIndex.map { group =>
        if(group._1 == '.' && validIndexes.contains(group._2)) 'X' else group._1
      }
      val updatedBottomRow = grid(rowNums._2).zipWithIndex.map { group =>
        if(group._1 == '.' && validIndexes.contains(group._2)) 'X' else group._1
      }
      grid(rowNums._1) = updatedTopRow.mkString("")
      grid(rowNums._2) = updatedBottomRow.mkString("")
      vertBombUpdate(grid, (rowNums._1 + 1, rowNums._2 + 1))
  }

  @tailrec
  def getIndexes(row: String, indexes: List[Int] = List(), counter: Int = 0): List[Int] = counter match {
    case _ if counter == row.length => indexes
    case _ =>
      if(row.charAt(counter) == 'O') {
        getIndexes(row, indexes :+ counter, counter + 1)
      } else {
        getIndexes(row, indexes, counter + 1)
      }
  }

  def horizBombUpdate(row: String): String = {
    val length = row.length
    val last = length - 1
    val secondToLast = length - 2

    @tailrec
    def recReplace(row: String, counter: Int = 0): String = {
      row.charAt(counter) match {
        case _ if counter == length => row
        case '.' => counter match {
          case `last` => row
          case `secondToLast` =>
            if(row.charAt(last) == 'O') row.dropRight(2) + "XO" else row
          case _ =>
            if(row.charAt(counter + 1) == 'O') {
              val updatedRow = row.substring(0, counter) + "X" + row.substring(counter + 1)
              recReplace(updatedRow, counter + 1)
            } else {
              recReplace(row, counter + 1)
            }
        }
        case 'O' => counter match {
          case `last` => row
          case `secondToLast` =>
            if(row.charAt(last) == 'O') row else row.dropRight(1) + "X"
          case _ =>
            if(row.charAt(counter + 1) == 'O') {
              recReplace(row, counter + 1)
            } else {
              val updatedRow = row.substring(0, counter + 1) + "X" + row.substring(counter + 2)
              recReplace(updatedRow, counter + 2)
            }
        }
      }
    }
    recReplace(row)
  }
}
