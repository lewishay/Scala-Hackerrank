//val sc = new java.util.Scanner(System.in)
//val t = sc.nextInt()
//var a0 = 0
//while(a0 < t) {
//  val s = sc.next()
//}

val s = "hefg"
val result = getLexiBigger(s, s)
if(s.equals(result)) println(s) else result

def getLexiBigger(s: String, result: String): String = n match {
  case _ if s.equals("") => result
  case _ => getLexiBigger(s.tail, nextBiggestFromCurrentPos(s))
}

def nextBiggestFromCurrentPos(s1: String): String = {
  ???
  //getLexiBigger(newString
}