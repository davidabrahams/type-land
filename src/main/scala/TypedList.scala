import Peano._

sealed trait List[+A] {
  type N <: Nat
}
object List {
  def add[A, N <: Nat](curr: List.Aux[A, N], other: List.Aux[A, N], f: (A, A) => A): List.Aux[A, N] =
    (curr, other) match {
      case (Cons(hc, tc), Cons(ho, to)) =>
        Cons(f(hc, ho), List.add(tc, to, f))
      case (Empty, Empty) => Empty
      case _ => sys.error("yah fuck")
    }

  type Aux[A0, N0 <: Nat] = List[A0] {
    type N = N0
  }
}

sealed trait Empty extends List.Aux[Nothing, _0]
case object Empty extends Empty

final case class Cons[A, N <: Nat](head: A, tail: List.Aux[A, N]) extends List.Aux[A, Succ[N]]

object Main {
  def main(args: Array[String]): Unit = {
    val list = Empty
    val l1 = Cons(2, Cons(0, Empty))
    val l2 = Cons(1, Cons(1, Cons(5, Empty)))
    val l3 = Cons(1, Cons(5, Empty))
    val func = (a: Int, b: Int) => a + b
    println(List.add(l1, l3, func))
    println("Failure!")
  }
}
