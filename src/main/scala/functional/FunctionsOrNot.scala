package functional

import java.util.Scanner
import scala.collection.mutable

// https://www.hackerrank.com/challenges/functions-or-not/problem

object FunctionsOrNot {

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val testCases = sc.nextLine().toInt
    (1 to testCases).foreach { _ =>
      val n = sc.nextLine().toInt
      val pairs: Seq[(Int, Int)] = (1 to n).map { _ =>
        sc.nextLine().split(" ") match {
          case Array(x, y) => (x.toInt, y.toInt)
        }
      }

      val functionalPairs = mutable.Map[Int, Int]()
      var answer = "YES"

      pairs.foreach { pair =>
        if (!functionalPairs.contains(pair._1)) {
          functionalPairs.put(pair._1, pair._2)
        } else {
          if (functionalPairs(pair._1) != pair._2) answer = "NO"
        }
      }

      println(answer)
    }
  }
}
