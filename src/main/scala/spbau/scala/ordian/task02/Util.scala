package spbau.scala.ordian.task02

import scala.collection.mutable

object Util {
  case class Memo[A,B](f: A => B) extends (A => B) {
    private val cache = mutable.Map.empty[A, B]
    def apply(x: A) = cache getOrElseUpdate (x, f(x))
  }
}
