val n = 5
val b = Array(2, 3, 4, 5, 6)
val x = Array(1, 2)

def fairRations(breadArray: Array[Int], counter: Int, maxIndex: Int): Int = counter match {
  case _ if maxIndex == 1 && breadArray.forall(_ % 2 != 0) && breadArray.forall(_ % 2 != 1) => 0
  case _ if breadArray.forall(_ % 2 == 0) => counter
  case _ =>
    val firstOdd = breadArray.find(_ % 2 != 0).head
    val indexOfFirstOdd = breadArray.indexOf(firstOdd)
    if(indexOfFirstOdd == 0) {
      fairRations(breadArray.updated(0, breadArray(0) + 1).updated(1, breadArray(1) + 1), counter + 2, maxIndex)
    } else if(indexOfFirstOdd == maxIndex) {
      fairRations(breadArray.updated(maxIndex, breadArray(maxIndex) + 1).updated(maxIndex - 1, breadArray(maxIndex - 1) + 1), counter + 2, maxIndex)
    } else {
      if(breadArray(indexOfFirstOdd - 1) < breadArray(indexOfFirstOdd + 1)) {
        fairRations(breadArray.updated(indexOfFirstOdd, breadArray(indexOfFirstOdd) + 1).updated(indexOfFirstOdd + 1, breadArray(indexOfFirstOdd  + 1) + 1), counter + 2, maxIndex)
      } else {
        fairRations(breadArray.updated(indexOfFirstOdd, breadArray(indexOfFirstOdd) + 1).updated(indexOfFirstOdd + 1, breadArray(indexOfFirstOdd  + 1) + 1), counter + 2, maxIndex)
      }
    }
}