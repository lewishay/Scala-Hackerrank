//val sc = new java.util.Scanner(System.in)
//val t = sc.nextInt()
//var a0 = 0
//while(a0 < t) {
//  val s = sc.next()
//}

val s = "hefg"
println(getLexiBigger(s))

def getLexiBigger(s: String): String = {
  val charPos = s.length - checkIfGreaterList(s.reverse, 1)
  if(charPos == 0) "no answer" else swapAndSort(s, charPos - 1)
}

def checkIfGreaterList(s: String, n: Int): Int = n match {
  case _ if n == s.length => n
  case _ => if(s.charAt(n) < s.charAt(n - 1)) n else checkIfGreaterList(s, n + 1)
}

def swapAndSort(s: String, c: Int): String = {
  val greaterChars = s.substring(c + 1).filter(_ > s.charAt(c))
  c match {
    case 0 => greaterChars(0) + s.substring(1).sorted
    case _ => s.substring(0, c) + greaterChars(0) + ???
  }
}
