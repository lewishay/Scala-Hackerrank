package algorithms.implementation

import scala.collection.mutable.ArrayBuffer

// See https://www.hackerrank.com/challenges/queens-attack-2/problem for a description of this problem.

object QueenAttack {

  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner (System.in)
    println("Enter the size of the board:")
    val n = sc.nextInt()
    println("Enter the number of obstacles:")
    val k = sc.nextInt()
    println("Enter the Queen's row number:")
    val rQueen = sc.nextInt()
    println("Enter the Queen's column number:")
    val cQueen = sc.nextInt()
    var a0 = 0
    val obstacles = ArrayBuffer.empty[(Int, Int)]
    while(a0 < k){
      println(s"Enter the row position of obstacle ${a0 + 1}:")
      val rObstacle = sc.nextInt()
      println(s"Enter the column position of obstacle ${a0 + 1}:")
      val cObstacle = sc.nextInt()
      obstacles += ((rObstacle, cObstacle))
      a0+=1
    }
    if(obstacles.isEmpty) {
      val horizontalAtk = (cQueen - 1) + (n - cQueen)
      val verticalAtk = (rQueen - 1) + (n - rQueen)
      val diagonalAtk = Math.min(rQueen - 1, cQueen - 1) +
        Math.min(rQueen - 1, n - cQueen) +
        Math.min(n - rQueen, cQueen - 1) +
        Math.min(n - rQueen, n - cQueen)
      println(s"The number of squares that the Queen can move to is: ${horizontalAtk + verticalAtk + diagonalAtk}")
    }
    else {
      val horizontalObstacles = obstacles.filter(x => x._1 == rQueen).map(x => x._2)
      val verticalObstacles = obstacles.filter(x => x._2 == cQueen).map(x => x._1)
      val leftPivot = getMax(horizontalObstacles.filter(x => x < cQueen))
      val rightPivot = getMin(horizontalObstacles.filter(x => x > cQueen), n)
      val bottomPivot = getMax(verticalObstacles.filter(x => x < rQueen))
      val topPivot = getMin(verticalObstacles.filter(x => x > rQueen), n)
      val horizontalAtk = (cQueen - leftPivot) + (rightPivot - cQueen)
      val verticalAtk = (rQueen - bottomPivot) + (topPivot - rQueen)

      val diagBottomLeftPivot = getDiagMax(
        validDiags(
          obstacles.filter(x => x._1 < rQueen && x._2 < cQueen), rQueen, cQueen
        )
      )
      val diagTopLeftPivot = getDiagMax(
        validDiags(
          obstacles.filter(x => x._1 > rQueen && x._2 < cQueen), rQueen, cQueen
        )
      )
      val diagBottomRightPivot = getDiagMin(
        validDiags(
          obstacles.filter(x => x._1 < rQueen && x._2 > cQueen), rQueen, cQueen
        )
      )
      val diagTopRightPivot = getDiagMin(
        validDiags(
          obstacles.filter(x => x._1 > rQueen && x._2 > cQueen), rQueen, cQueen
        )
      )
      val diagonalArray = Array(diagBottomLeftPivot, diagTopLeftPivot,
        diagBottomRightPivot, diagTopRightPivot)

      val btmLeft = diagonalArray(0) match {
        case (0, 0) => Math.min(rQueen - 1, cQueen - 1)
        case x => (cQueen - x._2) - 1
      }
      val topLeft = diagonalArray(1) match {
        case (0, 0) => Math.min(n - rQueen, cQueen - 1)
        case x => (cQueen - x._2) - 1
      }
      val btmRight = diagonalArray(2) match {
        case (0, 0) => Math.min(rQueen - 1, n - cQueen)
        case x => (x._2 - cQueen) - 1
      }
      val topRight = diagonalArray(3) match {
        case (0, 0) => Math.min(n - rQueen, n - cQueen)
        case x => (x._2 - cQueen) - 1
      }

      val diagonalAtk = btmLeft + topLeft + btmRight + topRight
      println(s"The number of squares that the Queen can move to is: ${horizontalAtk + verticalAtk + diagonalAtk}")
    }
  }

  def getMax(a: ArrayBuffer[Int]): Int = {
    if(a.isEmpty) 1 else a.max + 1
  }

  def getMin(a: ArrayBuffer[Int], n: Int): Int = {
    if(a.isEmpty) n else a.min - 1
  }

  def getDiagMax(a: ArrayBuffer[(Int, Int)]): (Int, Int) = {
    if(a.isEmpty || a.contains((0, 0))) (0, 0) else a.maxBy(_._2)
  }

  def getDiagMin(a: ArrayBuffer[(Int, Int)]): (Int, Int) = {
    if(a.isEmpty || a.contains((0, 0))) (0, 0) else a.minBy(_._2)
  }

  def validDiags(a: ArrayBuffer[(Int, Int)], r: Int, c: Int): ArrayBuffer[(Int, Int)] = {
    if (a.isEmpty) ArrayBuffer((0, 0)) else {
      val obstacles = ArrayBuffer.empty[(Int, Int)]
      a.foreach(x => if(Math.abs(r - x._1) == Math.abs(c - x._2)) obstacles += x)
      obstacles
    }
  }
}
