def isSorted(seq: Seq[Int], previousNum: Int = 0): Boolean = {
  if(seq.isEmpty) true else {
    if(seq.head >= previousNum) isSorted(seq.tail, seq.head) else false
  }
}

def swapElem(seq: Seq[Int], indexCount: Int = 0): String = {
  if(seq.lengthCompare(1) == 0) "fail" else {
    val (first, second) = (seq.head, seq(1))
    if(first > second) {
      val newList = Seq(second, first) ++ seq.drop(2)
      if(isSorted(newList)) {
        s"$indexCount ${indexCount + 1}"
      } else swapElem(seq.tail, indexCount + 1)
    } else swapElem(seq.tail, indexCount + 1)
  }
}

def reverseSection(seq: Seq[Int], indexCount: Int = 0, reversing: Boolean = false): String = {
  (seq.lengthCompare(1) == 0) "fail" else {
    val (first, second) = (seq.head, seq(1))
    if(first > second) {

    } else reverseSection(seq.tail, indexCount + 1, reversing)
  }
}

def almostSorted(seq: Seq[Int]): String = {
  if(isSorted(seq)) "yes" else {
    val swapResult = swapElem(seq)
    if(swapResult != "fail") s"yes\nswap $swapResult" else {
      val reverseResult = reverseSection(seq)
      if(reverseResult != "fail") s"yes\nreverse $reverseResult" else "no"
    }
  }
}

