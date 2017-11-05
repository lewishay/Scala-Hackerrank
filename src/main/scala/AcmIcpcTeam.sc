val n = 4
val m = 5
val topic = Array("00101", "11100", "00010", "10000")
println(getMaxTopicsTwoPerson(topic))
println(twoPersonTeamChecker(topic))

def getMaxTopicsTwoPerson(a: Array[String]): Int = {
  var currentMax = 0
  for(i <- a.indices) {
    for (j <- a.indices) {
      if(i <= j) {
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

def twoPersonTeamChecker(a: Array[String]): Int = {
  var counter = 0
  for(i <- a.indices) {
    for(j <- a.indices) {
      if(i <= j) {
        if(merger(a(i), a(j)).equals("1" * a(0).length)) counter += 1
      }
    }
  }
  counter
}
