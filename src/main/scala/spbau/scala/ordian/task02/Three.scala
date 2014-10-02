package spbau.scala.ordian.task02

object Three extends App {

  assert(Not(Not(X())) == X())
  assert(Not(Not(Not(X()))) == Or(Not(X()), And(Not(X()), Not(X()))))
  assert(Not(And(True(), Not(False()))) == False())
  assert(False() == And(And(Not(Or(X(), X())), X()), True()))

  abstract class Bool {
    def ==(that: Bool): Boolean = simplify(this) equals simplify(that)
  }

  case class And(left: Bool, right: Bool) extends Bool
  case class Or(left: Bool, right: Bool) extends Bool
  case class Not(op: Bool) extends Bool
  case class True() extends Bool
  case class False() extends Bool
  case class X() extends Bool


  /* clumsy */
  def simplify(b: Bool): Bool = b match {
    case And(left, right) => simplify(left) match {
      case False() => False()
      case True()  => simplify(right)
      case X()     => simplify(right) match {
        case False()  => False()
        case True()   => X()
        case Not(X()) => False()
        case X()      => X()
      }
      case Not(X())   => simplify(right) match {
        case False()  => False()
        case True()   => Not(X())
        case Not(X()) => Not(X())
        case X()      => False()
      }
    }
    case Or(left, right) => simplify(left) match {
      case True()  => True()
      case False() => simplify(right)
      case X()     => simplify(right) match {
        case True()   => True()
        case False()  => X()
        case Not(X()) => True()
        case X()      => X()
      }
      case Not(X())   => simplify(right) match {
        case True()   => True()
        case False()  => Not(X())
        case Not(X()) => Not(X())
        case X()      => True()
      }
    }
    case Not(op) => simplify(op) match {
      case True()           => False()
      case False()          => True()
      case Not(x)           => x
      case And(left, right) => simplify(Or(Not(left), Not(right)))
      case Or(left, right)  => simplify(And(Not(left), Not(right)))
      case X()              => Not(X())
    }
    case x@_ => x
  }
}
