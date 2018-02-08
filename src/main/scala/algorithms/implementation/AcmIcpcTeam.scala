package algorithms.implementation

// See https://www.hackerrank.com/challenges/acm-icpc-team/problem for a description of this problem.

object AcmIcpcTeam {

  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner (System.in)
    val n = sc.nextInt()
    val m = sc.nextInt()
    val topic = new Array[String](n)
    for(i <- 0 until n) {
      topic(i) = sc.next()
    }
    val maxTopics = getMaxTopicsTwoPerson(topic)
    println(maxTopics)
    println(twoPersonTeamChecker(topic, maxTopics))
  }

  def getMaxTopicsTwoPerson(a: Array[String]): Int = {
    var currentMax = 0
    for(i <- a.indices) {
      for(j <- a.indices) {
        if(i < j) {
          currentMax = Math.max(currentMax, merger(a(i), a(j)).count(_.equals('1')))
        }
      }
    }
    currentMax
  }

  def merger(s1: String, s2: String): String = {
    val result = new StringBuilder(s2)
    for(i <- s1.indices) {
      if(s1.charAt(i).equals('1')) result.setCharAt(i, '1')
    }
    result.toString
  }

  def twoPersonTeamChecker(a: Array[String], m: Int): Int = {
    var counter = 0
    for(i <- a.indices) {
      for(j <- a.indices) {
        if(i < j) {
          if(merger(a(i), a(j)).count(_.equals('1')) == m) counter += 1
        }
      }
    }
    counter
  }

}
