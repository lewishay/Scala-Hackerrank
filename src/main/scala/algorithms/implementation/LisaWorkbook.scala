package algorithms.implementation

// See https://www.hackerrank.com/challenges/lisa-workbook/problem for a description of this problem

object LisaWorkbook {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    println("Enter the number of chapters:")
    val n = sc.nextInt()
    println("Enter the max number of problems per page:")
    val k = sc.nextInt()
    val arr = new Array[Int](n)
    println("Enter the number of problems in each chapter, one by one:")
    for(i <- 0 until n) {
      arr(i) = sc.nextInt()
    }
    val result = workbook(k, arr.toList)
    println(s"The number of special chapters is: $result")
  }

  def workbook(k: Int, list: List[Int], currentPage: Int = 1, numSpecials: Int = 0): Int = {
    if(list.isEmpty) {
      numSpecials
    } else {
      val specials = isSpecial(k, currentPage, list.head)
      workbook(k, list.tail, currentPage + pagesNeeded(list.head, k), numSpecials + specials)
    }
  }

  def isSpecial(k: Int, currentPage: Int, numChapters: Int, chapterCount: Int = 1, numSpecial: Int = 0): Int = {
    if(numChapters < k) {
      val specials = if((chapterCount until chapterCount + numChapters).contains(currentPage)) 1 else 0
      numSpecial + specials
    } else {
      val specials = if((chapterCount until chapterCount + k).contains(currentPage)) 1 else 0
      isSpecial(k, currentPage + 1, numChapters - k, chapterCount + k, numSpecial + specials)
    }
  }

  def pagesNeeded(chapters: Int, k: Int, pages: Int = 0): Int = chapters match {
    case 0 => pages
    case _ if chapters < k => pages + 1
    case _ => pagesNeeded(chapters - k, k, pages + 1)
  }
}
