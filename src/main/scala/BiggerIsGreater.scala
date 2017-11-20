// See https://www.hackerrank.com/challenges/bigger-is-greater/problem for a description of this problem

object BiggerIsGreater {

  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    println("Please enter a word:")
    val s = sc.next()
    println("The next lexicographically greater word is: " + getLexiBigger(s))
  }

  def getLexiBigger(s: String): String = {
    val charPos = s.length - checkIfGreaterList(s.reverse, 1)
    if(charPos == 0) "no answer" else swapAndSort(s, charPos - 1)
  }

  def checkIfGreaterList(s: String, n: Int): Int = n match {
    case _ if n == s.length => n
    case _ => if(s.charAt(n) < s.charAt(n - 1)) n else checkIfGreaterList(s, n + 1)
  }

  def swapAndSort(s: String, c: Int): String = {
    val greaterChars = s.substring(c + 1).filter(_ > s.charAt(c)).sorted
    c match {
      case 0 => greaterChars(0) + s.replaceFirst(greaterChars(0).toString, "").sorted
      case _ => s.substring(0, c) + greaterChars(0) + s.substring(c).replaceFirst(greaterChars(0).toString, "").sorted
    }
  }
}
