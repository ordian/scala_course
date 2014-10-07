package spbau.scala.ordian.task02

import scala.annotation.tailrec

/* WARNING: magic constants! */
object One extends App {

  print(f"${squareRootDigits(2).head}.")
  squareRootDigits(2).tail take 73 foreach print

  def squareRootDigits(number: Int): Stream[Int] = {

    def digitByDigit(remainder: BigInt, result: BigInt): Stream[Int] = {
      val (digit, newRemainder) = next(0, remainder, result * 20 + 1)
      digit #:: digitByDigit(newRemainder, result * 10 + digit)
    }

    @tailrec
    def next(digit: Int, remainder: BigInt, decrement: BigInt): (Int, BigInt) = {
      if (remainder < decrement) (digit, remainder * 100)
      else next(digit + 1, remainder - decrement, decrement + 2)
    }

    digitByDigit(number, 0)
  }
}
