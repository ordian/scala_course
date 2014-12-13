package spbau.scala.ordian.task04

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.Context

object Two {
  def myPrintf(format: String, params: Any*): Unit = macro printfImpl

  def printfImpl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val Literal(Constant(s_format: String)) = format.tree
    val vals = mutable.ArrayBuffer[ValDef]()
    val stack = mutable.Stack[Tree](params map (_.tree): _*)

    def go(v: Tree, t: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      vals += ValDef(Modifiers(), freshName, TypeTree(t), v)
      Ident(freshName)
    }

    val refs = s_format.split("(?<=%[\\w%])|(?=%[\\w%])") map {
      case "%d" => go(stack.pop(), typeOf[Int])
      case "%s" => go(stack.pop(), typeOf[String])
      case "%c" => go(stack.pop(), typeOf[Char])
      case "%f" => go(stack.pop(), typeOf[Double])
      case "%%" => Literal(Constant("%"))
      case x    => Literal(Constant(x))
    }

    val stats = vals ++ refs.map(ref => reify(print(c.Expr[Any](ref).splice)).tree)
    c.Expr[Unit](Block(stats.toList, Literal(Constant(()))))
  }
}
