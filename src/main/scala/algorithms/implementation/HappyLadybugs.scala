package algorithms.implementation

// See https://www.hackerrank.com/challenges/happy-ladybugs/problem for a description of this problem.

object HappyLadybugs {
  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    println("Enter the string:")
    val b = sc.next()
    val result = if(happyLadybugs(b)) "All the ladybugs are happy :)" else "There are unhappy ladybugs :("
    println(result)
  }

  def happyLadybugs(gameBoard: String): Boolean = {
    if(!gameBoard.contains('_')) {
      consecutiveCharChecker(gameBoard)
    } else {
      def ladybugRec(board: String): Boolean = board match {
        case _ if board.isEmpty => true
        case _ =>
          if(board.count(_ == board.head) > 1) {
            ladybugRec(board.replaceAll(board.head.toString, ""))
          } else false
      }
      ladybugRec(gameBoard.replaceAll("_", ""))
    }
  }

  def consecutiveCharChecker(fullString: String, index: Int = 0, flag: Boolean = false): Boolean = {
    index match {
      case _ if fullString.length == 1 => false
      case _ if index == fullString.length - 2 => checkerHelper(fullString, index)
      case 0 =>
        if(checkerHelper(fullString, index)) {
          consecutiveCharChecker(fullString, index + 1)
        } else false
      case _ if flag && !checkerHelper(fullString, index) => false
      case _ =>
        if(checkerHelper(fullString, index)) {
          consecutiveCharChecker(fullString, index + 1)
        } else {
          consecutiveCharChecker(fullString, index + 1, flag = true)
        }
    }
  }

  def checkerHelper(theString: String, index: Int): Boolean = theString(index) == theString(index + 1)
}
