package algorithms.implementation

// See https://www.hackerrank.com/challenges/absolute-permutation/problem for a description of this problem.

object AbsolutePermutation {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    println("Please enter the value of N, which defines the list of all natural numbers [1 to N]:")
    val n = sc.nextInt()
    println("Please enter the value of K, which all numbers must evaluate to:")
    val k = sc.nextInt()
    println(permutations(n, k))
  }

  def permutations(n: Int, k: Int): String = {
    (n, k) match {
      case (_, 0) => List.range(1, n + 1).mkString(" ")
      case _ if n % (2 * k) != 0 => "Sorry, this list is not an absolute permutation"
      case _ => buildList(k, n + 1).mkString(" ")
    }
  }

  def buildList(k: Int, n: Int): List[Int] = {
    @annotation.tailrec
    def recBuild(i: Int, incrementing: Boolean, list: List[Int] = List()): List[Int] = i match {
      case `n` => list
      case _ if incrementing =>
        if(i % k == 0) {
          recBuild(i + 1, incrementing = false, (i + k) :: list)
        } else {
          recBuild(i + 1, incrementing = true, (i + k) :: list)
        }
      case _ if !incrementing =>
        if(i % k == 0) {
          recBuild(i + 1, incrementing = true, (i - k) :: list)
        } else {
          recBuild(i + 1, incrementing = false, (i - k) :: list)
        }
    }
    recBuild(1, incrementing = true).reverse
  }
}
