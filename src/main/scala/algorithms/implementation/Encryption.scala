package algorithms.implementation

import scala.collection.mutable.ArrayBuffer

// See https://www.hackerrank.com/challenges/encryption/problem for a description of this problem.
object Encryption {

  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    println("Please input a sentence:")
    val s = sc.nextLine().replace(" ", "")
    println(encrypt(populateGrid(s), 0))
  }

  def getRowsAndCols(n: Int): (Int, Int) = {
    if(Math.sqrt(n) % 1 == 0) {
      (Math.sqrt(n).toInt, Math.sqrt(n).toInt)
    } else {
      (Math.sqrt(n).toInt, Math.sqrt(n).toInt + 1)
    }
  }

  def populateGrid(s: String): Array[String] = {
    val (rows, columns) = getRowsAndCols(s.length)
    val grid = new ArrayBuffer[String]
    for(i <- 0 to rows) i match {
      case 0 => grid += s.substring(0, columns)
      case _ if(columns * i) > s.length =>
      case _ if((columns * i) + columns) > s.length => grid += s.substring(columns * i)
      case _ => grid += s.substring(columns * i, (columns * i) + columns)
    }
    grid.toArray
  }

  def encrypt(a: Array[String], n: Int): String = {
    if(n > a.length) "" else getColumnWord(a, n) + " " + encrypt(a, n + 1)
  }

  def getColumnWord(a: Array[String], i: Int): String = {
    a.map(elem => if(i < elem.length) elem.charAt(i)).mkString.replace("()", "")
  }
}
