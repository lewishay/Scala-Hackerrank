package algorithms.implementation

// See https://www.hackerrank.com/challenges/the-time-in-words/problem for a description of this problem

object TimeInWords {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    println("Enter the hour:")
    val h = sc.nextInt()
    println("Enter the minute:")
    val m = sc.nextInt()
    println(s"The time is ${timeInWords(h, m)}.")
  }

  def timeInWords(hour: Int, minute: Int): String = minute match {
    case 0 => s"${numToWord(hour)} o' clock"
    case 1 => s"one minute past ${numToWord(hour)}"
    case 15 => s"quarter past ${numToWord(hour)}"
    case 30 => s"half past ${numToWord(hour)}"
    case 45 => s"quarter to ${numToWord(hour + 1)}"
    case 59 => s"one minute to ${numToWord(hour + 1)}"
    case _ if minute > 30 => s"${numToWord(60 - minute)} minutes to ${numToWord(hour + 1)}"
    case _ if minute < 30 => s"${numToWord(minute)} minutes past ${numToWord(hour)}"
  }

  def numToWord(number: Int): String = number match {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
    case 10 => "ten"
    case 11 => "eleven"
    case 12 => "twelve"
    case 13 => "thirteen"
    case 15 => "fifteen"
    case 20 => "twenty"
    case 30 => "thirty"
    case 40 => "forty"
    case 50 => "fifty"
    case x if x > 50 => s"fifty ${numToWord(x % 10)}"
    case x if x > 40 => s"forty ${numToWord(x % 10)}"
    case x if x > 30 => s"thirty ${numToWord(x % 10)}"
    case x if x > 20 => s"twenty ${numToWord(x % 10)}"
    case x => s"${numToWord(x % 10)}teen"
  }
}
