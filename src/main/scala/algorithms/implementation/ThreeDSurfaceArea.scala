package algorithms.implementation

// See https://www.hackerrank.com/challenges/3d-surface-area/problem for a description of this problem.

object ThreeDSurfaceArea {
  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    println("Enter the height of the grid:")
    val height = sc.nextInt()
    println("Enter the width of the grid:")
    val width = sc.nextInt()
    println("Enter the heights of each element for each row, seperated by spaces:")
    val arr = Array.ofDim[Int](height, width)
    for(i <- 0 until height) {
      for(j <- 0 until width){
        arr(i)(j) = sc.nextInt()
      }
    }
    val result = surfaceArea(arr, height, width)
    println(s"The surface area is: $result")
  }

  def surfaceArea(arr: Array[Array[Int]], height: Int, width: Int): Int = {
    val topAndBottomArea = (height * width) * 2
    val frontFace = arr.indices.map(x => arr(x)(0)).sum + middleAreas("front", arr)
    val backFace = arr.indices.map(x => arr(x)(width - 1)).sum + middleAreas("back", arr)
    val leftFace = arr.head.sum + middleAreas("left", arr)
    val rightFace = arr.last.sum + middleAreas("right", arr)
    topAndBottomArea + frontFace + backFace + leftFace + rightFace
  }

  def middleAreas(direction: String, arr: Array[Array[Int]], iter: Int = 0, total: Int = 0): Int = direction match {
    case _ if (direction == "right" || direction == "left") && iter == arr.head.length => total
    case _ if (direction == "front" || direction == "back") && iter == arr.length => total
    case "front" =>
      val rowFaces = frontOrBackFaces(arr(iter), arr.head.length - 2)
      middleAreas("front", arr, iter + 1, total + rowFaces)
    case "left" =>
      val rowFaces = (0 to arr.length - 2).map { x =>
        if(arr(x)(iter) < arr(x + 1)(iter)) arr(x + 1)(iter) - arr(x)(iter) else 0
      }.sum
      middleAreas("left", arr, iter + 1, total + rowFaces)
    case "right" =>
      val rowFaces = (arr.length - 1 to 1 by -1).map { x =>
        if(arr(x)(iter) < arr(x - 1)(iter)) arr(x - 1)(iter) - arr(x)(iter) else 0
      }.sum
      middleAreas("right", arr, iter + 1, total + rowFaces)
    case "back" =>
      val rowFaces = frontOrBackFaces(arr(iter).reverse, arr.head.length - 2)
      middleAreas("back", arr, iter + 1, total + rowFaces)
  }

  def frontOrBackFaces(currentRow: Array[Int], height: Int): Int = {
    (0 to height).map { x =>
      if(currentRow(x) < currentRow(x + 1)) currentRow(x + 1) - currentRow(x) else 0
    }.sum
  }
}
