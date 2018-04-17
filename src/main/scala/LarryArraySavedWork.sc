val n = 3
val a = Array(3, 1, 2)

def larrysArray(a: Array[Int]): String = {
  val sortedArray = a.sorted

  def recFunc(arr: Array[Int], currentNum: Int = 1, prevArr: Array[Int] = Array()): String = arr match {
    case _ if arr.sameElements(sortedArray) => "YES"
    case _ if arr.sameElements(prevArr) => "NO"
    case _ if arr.head == currentNum => recFunc(arr, currentNum + 1, prevArr)
    case _ =>
  }
  recFunc(a)
}

def rotate(elem1: Int, elem2: Int, elem3: Int): (Int, Int, Int) = {
  val smallest = Math.min(Math.min(elem1, elem2), elem3)
  (elem2 == smallest, elem3 == smallest) match {
    case (true, false) => (elem2, elem3, elem1)
    case (false, true) => (elem3, elem1, elem2)
    case _ => (elem1, elem2, elem3)
  }
}
