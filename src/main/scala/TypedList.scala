import Peano._

sealed trait TList[+A] {
  type N <: Nat
}
object TList {
  def add[A, N <: Nat](curr: TList.Aux[A, N],
                       other: TList.Aux[A, N],
                       f: (A, A) => A): TList.Aux[A, N] =
    ((curr, other) match {
      case (Cons(hc, tc), Cons(ho, to)) =>
        Cons(f(hc, ho), TList.add(tc, to, f))
      case (Empty, Empty) => Empty
    }).asInstanceOf[TList.Aux[A, N]]

  type Aux[A0, N0 <: Nat] = TList[A0] { type N = N0 }
}

sealed trait Empty extends TList[Nothing] {
  type N = _0
  def ::[A](v: A) = Cons(v, this)
}
case object Empty extends Empty

final case class Cons[A, P <: Nat](head: A, tail: TList.Aux[A, P])
    extends TList[A] { cons =>
  def ::(v: A): Cons[A, N] { type N = Succ[cons.N] } = Cons(v, this)
  type N = Succ[P]
}

object Main {
  def main(args: Array[String]): Unit = {
    val list = Empty
    val l1 = 2 :: 0 :: Empty
    val l2 = 1 :: 1 :: 5 :: Empty
    val l3 = 1 :: 5 :: Empty
    val func = (a: Int, b: Int) => a + b
    println(TList.add(l1, l3, func))
  }
}
