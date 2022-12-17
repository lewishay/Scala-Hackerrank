package algorithms.implementation

import scala.io.StdIn.readLine

object NonDivisibleSubset {

  def nonDivisibleSubset(k: Int, s: Array[Int]): Int = k match {
    case 1 => 1
    case _ =>
      val remainders = s.map(_ % k)
      val combinations = remainders.combinations(2)
      val pairsDivisibleByK = combinations.filter(x => (x(0) + x(1)) % k == 0)
      val numsToEliminate = pairsDivisibleByK.map { pair =>
        val firstCount = remainders.count(_ == pair(0))
        val secondCount = remainders.count(_ == pair(1))
        (firstCount, secondCount) match {
          case _ if pair(0) == pair(1) => Seq.fill(firstCount - 1)(pair(0))
          case (f, s) if f > s => Seq.fill(secondCount)(pair(1))
          case _ => Seq.fill(firstCount)(pair(0))
        }
      }.toSeq.flatten

      remainders.diff(numsToEliminate).length
  }

  def main(args: Array[String]): Unit = {
    val firstLine = readLine().split(" ")
    val k = firstLine(1).toInt
    val s = readLine().split(" ").map(_.toInt)
    val result = nonDivisibleSubset(k, s)

    println(result)
  }
}
