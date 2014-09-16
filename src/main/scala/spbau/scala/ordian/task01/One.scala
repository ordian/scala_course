package spbau.scala.ordian.task01

import scala.annotation.tailrec

object One extends App {
  assert(gcd(-14, 21) == 7)
  assert(gcd(1, 42) == 1)

  assert(fib(5) == 8)
  assert(fib(500) == fib(500, linear = false))

  assert(qsort(List(5,2,1,4,3)) equals List(1,2,3,4,5))

  /* 1 */
  @tailrec
  final def gcd(a: Int, b: Int): Int = b match {
    case 0 => a.abs
    case _ => gcd(b, a % b)
  }

  /* 2 */
  def fib(n: Int, linear: Boolean = true): Long = {
    require(n >= 0)

    @tailrec
    def fibLinear(n: Int, a: Long, b: Long): Long= {
      if (n == 0) a
      else fibLinear(n - 1, a + b, a)
    }

    /*
     * F(2n)     = (2F(n - 1) + F(n)) * F(n)
     * F(2n - 1) = F(n)^2 + F(n-1)^2
    */
    def fibLog(n: Int): (Long, Long) = {
      assert(n >= 1)
      if (n == 1) (1, 0)
      else {
        val (x, y) = fibLog(n / 2)
        val a = (2 * y + x) * x
        val b = x * x + y * y
        if (isEven(n)) (a, b)
        else (a + b, a)
      }
    }

    if (linear) fibLinear(n, 1, 0)
    else fibLog(n + 1)._1
  }

  /* 3 */
  def qsort(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case x :: xs =>
      val (left, right) = xs partition (x > _)
      qsort(left) ::: x :: qsort(right)
  }

}
