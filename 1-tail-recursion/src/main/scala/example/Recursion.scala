package example

import scala.annotation.tailrec

object Recursion {
  def factorial(n: Int): Int = {
    @tailrec
    def factorial_int(n: Int, acc: Int): Int =
      if (n <= 1) acc else factorial_int(n-1, acc * n)

    factorial_int(n, 1)
  }
}