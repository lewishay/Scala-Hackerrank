package algorithms.implementation

// See https://www.hackerrank.com/challenges/kaprekar-numbers/problem for a description of this problem

object KaprekarNumbers {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    println("Enter the first number:")
    val p = sc.nextInt()
    println("Enter the second number:")
    val q = sc.nextInt()
    val result = getKaprekarNums(p, q)
    if(result.isEmpty) println("INVALID RANGE") else println(s"The Kaprekar number between $p and $q are: $result")
  }

  def getKaprekarNums(i1: Int, i2: Int): String = {
    (i1 to i2).filter(x => kaprekarCheck(x)).mkString(" ")
  }

  def kaprekarCheck(n: Int): Boolean = {
    val squared = (n.toLong * n.toLong).toString
    squared match {
      case _ if squared.equals("1") => true
      case _ if squared.length < 2 => false
      case _ => if(squared.substring(0, squared.length/2).toInt +
        squared.substring(squared.length/2).toInt == n) true else false
    }
  }
}
