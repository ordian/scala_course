package spbau.scala.ordian.task03

import scala.util.parsing.combinator._

abstract class Expression
case class Add(left: Expression, right: Expression) extends Expression
case class Sub(left: Expression, right: Expression) extends Expression
case class Mul(left: Expression, right: Expression) extends Expression
case class Div(left: Expression, right: Expression) extends Expression
case class Num(value: Double) extends Expression

object ExpressionEvaluator {
  def run(e: Expression): Double = e match {
    case Num(v) => v
    case Add(l, r) => run(l) + run(r)
    case Sub(l, r) => run(l) - run(r)
    case Mul(l, r) => run(l) * run(r)
    case Div(l, r) => run(l) / run(r)
  }
}

/*
* LL(k) grammar for reverse polish arithmetic:
*   E -> SR | n
*   S -> nEo
*   R -> EoR | epsilon
* n is number
* o is operation
*/
object UglyReversePolishParser extends JavaTokenParsers {
  type Operation = (Expression, Expression) => Expression

  def e: Parser[Expression] = s ~ r ^^ {
    case s ~ r => r.foldLeft(s)({ case (c, (t, o)) => o(c, t) })
  } | n
  def s: Parser[Expression] = n ~ e ~ o ^^ { case n ~ e ~ o => o(n, e) }
  def r: Parser[List[(Expression, Operation)]] = rep(e ~ o) ^^ {
    case terms => terms.map({ case e ~ o => (e, o) })
  }
  def n: Parser[Num] = floatingPointNumber ^^ { d => Num(d.toDouble)}
  def o: Parser[Operation] = ("+" | "-" | "*" | "/") ^^ {
    case "+" => Add
    case "-" => Sub
    case "*" => Mul
    case "/" => Div
  }
  def run(s: String) = parseAll(e, s)
}


object One extends App {
  val result = UglyReversePolishParser.run("5 1 2 + 4 * + 3 -")
  println(result)
  println(ExpressionEvaluator.run(result.getOrElse(Num(Double.NaN))))
}
