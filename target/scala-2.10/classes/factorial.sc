import scala.annotation.tailrec

def fact(n: Int): Long = {
  require(n >= 0)

  @tailrec
  def withAcc(n: Int, acc: Long): Long = {
    if (n == 0) acc
    else withAcc(n - 1, n * acc)
  }

  withAcc(n, 1)
}

fact(2)

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def tail: List[T] = throw new NoSuchElementException

  override def head: T = throw new NoSuchElementException
}

def singleton[T](elem: T): List[T] = new Cons(elem, new Nil)

singleton(1)


def nth[T](n: Int, xs: List[T]): T = {
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) xs.head
  else nth(n - 1, xs.tail)
}

