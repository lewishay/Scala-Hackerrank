def isSorted(seq: Seq[Int], previousNum: Int = 0): Boolean = {
  if(seq.isEmpty) true else {
    if(seq.head >= previousNum) isSorted(seq.tail, seq.head) else false
  }
}

def swapElem(seq: Seq[Int], leftIndex: Int, rightIndex: Int): Seq[Int] =
  seq.patch(leftIndex, Seq(seq(rightIndex)), 1).patch(rightIndex, Seq(seq(leftIndex)), 1)

def reverseSection(seq: Seq[Int], leftIndex: Int, rightIndex: Int): Seq[Int] = {
  val sectionToReverse = seq.slice(leftIndex, rightIndex).reverse
  seq.take(leftIndex) ++ sectionToReverse ++ seq.drop(rightIndex)
}

def getIndexLeft(seq: Seq[Int], indexCount: Int = 0): Option[Int] = seq match {
  case Seq(int1, int2) =>
    if(int2 < int1) Some(indexCount) else None
  case Seq(int1, int2, _*) if int2 < int1 => Some(indexCount)
  case _ => getIndexLeft(seq.tail, indexCount + 1)
}

def getIndexRight(seq: Seq[Int], indexCount: Int = 0): Option[Int] = seq match {
  case Seq(int1, int2) =>
    if(int2 > int1) Some(indexCount) else None
  case Seq(int1, int2, _*) if int2 > int1 => Some(indexCount)
  case _ => getIndexRight(seq.tail, indexCount + 1)
}

def almostSorted(seq: Seq[Int]): String = {
  if(isSorted(seq)) "yes" else {
    val leftIndex = getIndexLeft(seq)
    val rightIndex = getIndexRight(seq.reverse).map(seq.length - _)
    (leftIndex, rightIndex) match {
      case (Some(lIndex), Some(rIndex)) =>
        println(s"L: $lIndex, R: $rIndex")
        if(isSorted(swapElem(seq, lIndex, rIndex))) {
          s"yes\nswap ${lIndex + 1} $rIndex"
        } else if(isSorted(reverseSection(seq, lIndex, rIndex))) {
          s"yes\nreverse ${lIndex + 1} $rIndex"
        } else {
          "no"
        }
      case _ => "no"
    }
  }
}

almostSorted(Seq(4, 2))

//Current state: index out of bounds when the right index is the last element