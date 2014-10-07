package spbau.scala.ordian.task02

import spbau.scala.ordian.task02.Util.Memo

object Four extends App {

  solve(42) take 42 foreach println

  abstract class Expression {
    def value: Int
  }
  case class Num(value: Int) extends Expression {
    override def toString: String = value.toString
  }
  case class Op(left: Expression, op: Char, right: Expression) extends Expression {
    override def toString: String = s"($left $op $right)"
    override val value: Int = op match {
      case '+' => left.value + right.value
      case '-' => left.value - right.value
      case '*' => left.value * right.value
      case _ => throw new UnsupportedOperationException
    }
  }

  /* slow */
  def solve(number: Int, numbers: Seq[Int] = List.range(1, 11), operations: Seq[Char] = "+-*"): Stream[Expression] = {
    def generate: Memo[(Int, Int), Stream[Expression]] = Memo {
      case (i, j) =>
        val length = j - i
        if (length == 1) {
          Num(numbers(i)) #:: Stream.empty
        } else {
          (1 until length).toStream flatMap (k => {
            val (left, right) = (generate(i, i + k), generate(i + k, j))
            for {l <- left; op <- operations.toStream; r <- right} yield Op(l, op, r)
          })
        }
    }
    generate(0, numbers.length) filter (_.value == number)
  }
}
