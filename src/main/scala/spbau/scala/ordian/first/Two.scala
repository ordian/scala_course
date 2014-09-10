package spbau.scala.ordian.first

import spbau.scala.ordian.first.Util.isEven

import scala.util.Random

abstract class Scheme
case class Res(v: Int) extends Scheme
case class Par(s1: Scheme, s2: Scheme) extends Scheme
case class Seqq(s1: Scheme, s2: Scheme) extends Scheme

object Scheme {
  private val delimiter = 3

  def show(s: Scheme): String = {
    val sb = new StringBuilder
    toStringList(s) foreach (sb.append(_).append("\n"))
    sb.result()
  }

  def generate(limit: Int): Scheme = {
    val i = Random.nextInt(8)
    if (i == 0 || limit == 0) Res(Random.nextInt(142))
    else if (i < 4) Par(generate(limit - 1), generate(limit - 1))
    else Seqq(generate(limit - 1), generate(limit - 1))
  }

  private /* or implicit */ def bool2Int(b: Boolean) = if (b) 1 else 0

  private def toStringList(s: Scheme): List[String] = s match {
    case Res(v) =>
      val s = v.toString
      val size = s.length / 2
      val space = " " * size
      val connector = space + "|" + space
      connector :: s + (" " * bool2Int(isEven(s.length)))  :: connector :: List()
    case Par(s1, s2) =>
      val (a, b) = (toStringList(s1), toStringList(s2))
      val (i, j) = (a(0).length, b(0).length)
      val header = (new StringBuilder)
        .append(" " * (i / 2 )).append("+").append("-" * (i / 2))
        .append("-" * delimiter)
        .append("-" * (j / 2)).append("+").append(" " * (j / 2))
        .result()
      val space = " " * ((i + j + delimiter) / 2)
      val connector = space + "|" + space
      connector :: header :: merge(a, b, " " * delimiter) ::: header :: connector :: List()
    case Seqq(s1, s2) =>
      val (a, b) = (toStringList(s1), toStringList(s2))
      val (i, j) = (a(0).length, b(0).length)
      val space = " " * ((j - i) / 2).abs
      if (i < j) {
        wrapEach(a, space) ::: b.tail
      } else if (j < i) {
        a ::: wrapEach(b, space).tail
      } else {
        a ::: b.tail
      }
    case _ => throw new UnsupportedOperationException
  }

  private def wrapEach(xs: List[String], w: String): List[String] = xs map (w + _ + w)

  private def merge(a: List[String], b: List[String], separator: String): List[String] = {
    val (i, j) = (a.size, b.size)
    val (x, y) =
      if (i < j) (wrap(a, a(0), (j - i) / 2), b)
      else if (j < i) (a, wrap(b, b(0), (i - j) / 2))
      else (a, b)
    (x, y).zipped map (_ + separator + _)
  }

  private def wrap(xs: List[String], s: String, times: Int): List[String] = {
    val w = List.fill(times)(s)
    w ::: xs ::: w
  }
}

object Two extends App {
  println(Scheme.show(Scheme.generate(5)))
}
