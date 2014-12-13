package spbau.scala.ordian.task04

import spbau.scala.ordian.task04.One.HList.:::

object One {

  def main(args: Array[String]) {
    import HList._

    def indexAt2ofT[A, B, T <: HList](x: (A :: B :: T)) = x match {
      case a :: b :: c :: _ => c
    }

    val list1 = 1 :: false :: "Hi"  :: HNil
    val list2 = 2 :: true  :: "Bye" :: HNil
    println(indexAt2ofT( list2 ::: (1 :: HNil) ::: list1 ::: list2))
  }

  trait Fold[T, V] {
    type Apply[N <: T, Acc <: V] <: V
    def apply[N <: T, Acc <: V](n: N, acc: Acc): Apply[N, Acc]
  }

  sealed trait HList {
    type Foldr[V, F <: Fold[Any, V], I <: V] <: V
    def foldr[V, F <: Fold[Any, V], I <: V](f: F, i: I): Foldr[V, F, I]
  }

  final case class HCons[H, T <: HList](head: H,
                                        tail: T) extends HList {
    def ::[E](v: E) = HCons(v, this)
    override def toString = head + " :: " + tail

    type Foldr[V, F <: Fold[Any, V], I <: V] = F#Apply[H, tail.Foldr[V, F, I]]
    def foldr[V, F <: Fold[Any, V], I <: V](f: F, i: I): Foldr[V, F, I] =
      f(head, tail.foldr[V, F, I](f, i))
  }

  final class HNil extends HList {
    def ::[T](v: T) = HCons(v, this)
    override def toString = "Nil"

    type Foldr[V, F <: Fold[Any, V], I <: V] = I
    def foldr[V, F <: Fold[Any, V], I <: V](f: F, i: I) = i
  }

  object HList {
    type ::[H, T <: HList] = HCons[H, T]
    val :: = HCons
    val HNil = new HNil

    type :::[A <: HList, B <: HList] = A#Foldr[HList, FoldHCons.type, B]
    implicit def concat[B <: HList](b: B): Concat[B] =
      new Concat[B] {
        def :::[A <: HList](a: A): A#Foldr[HList, FoldHCons.type, B] =
          a.foldr[HList, FoldHCons.type, B](FoldHCons, b)
      }

    object FoldHCons extends Fold[Any, HList] {
      type Apply[N <: Any, H <: HList] = N :: H
      def apply[A, B <: HList](a: A, b: B) = HCons(a, b)
    }

  }

  sealed trait Concat[B <: HList] {
    def :::[A <: HList](a: A): A ::: B
  }
}
