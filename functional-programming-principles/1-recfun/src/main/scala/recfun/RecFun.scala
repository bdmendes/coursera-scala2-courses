package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balance_iter(chars: List[Char], open_count: Int): Boolean = {
      if (chars.isEmpty) open_count == 0
      else if (open_count < 0) false
      else {
        val new_count = chars.head match {
          case '(' => open_count + 1
          case ')' => open_count - 1
          case _ => open_count
        }
        balance_iter(chars.tail, new_count)
      }
    }

    balance_iter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      val pickCoin = countChange(money - coins.head, coins)
      val skipCoin = countChange(money, coins.tail)
      pickCoin + skipCoin
    }
  }
}
