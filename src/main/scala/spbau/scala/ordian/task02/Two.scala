package spbau.scala.ordian.task02

object Two extends App {

  pascalsTriangle take 6 foreach { l => l foreach { printf("%d ", _) }; println() }

  def pascalsTriangle: Stream[List[Int]] = List(1) #:: List(1, 1) #::
    pascalsTriangle.tail.zip(pascalsTriangle.tail).map { n => 1 :: sum(n._1, n._2.tail) ::: List(1) }

  def sum(xs: List[Int], ys: List[Int]): List[Int] = xs zip ys map { case (a, b) => a + b }
}
