package algorithms.implementation

// See https://www.hackerrank.com/challenges/strange-code/problem for a description of this problem.

object StrangeCounter {
  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    println("Please enter the number:")
    val t = sc.nextLong()
    println(strangeCode(t))
  }

  def strangeCode(t: Long): Long = {
    if(t > 3) {
      (t - counterGroupMax(t) - 1L) * -1L
    } else {
      Seq(3L, 2L, 1L).indexOf(t) + 1L
    }
  }

  @annotation.tailrec
  def counterGroupMax(limit: Long, inc: Long = 6L, currentMax: Long = 3L): Long = currentMax match {
    case x if x >= limit => x
    case x => counterGroupMax(limit, inc * 2L, x + inc)
  }
}
