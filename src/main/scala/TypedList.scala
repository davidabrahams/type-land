import Peano._

sealed trait List[+A] {
  type N <: Nat
}
object List {
  def add[A, N <: Nat](curr: List.Aux[A, N],
                       other: List.Aux[A, N],
                       f: (A, A) => A): List.Aux[A, N] =
    ((curr, other) match {
      case (Cons(hc, tc), Cons(ho, to)) =>
        Cons(f(hc, ho), List.add(tc, to, f))
      case (Empty, Empty) => Empty
    }).asInstanceOf[List.Aux[A, N]]

  type Aux[A0, N0 <: Nat] = List[A0] { type N = N0 }
}

sealed trait Empty extends List[Nothing]
case object Empty extends Empty

final case class Cons[A, P <: Nat](head: A, tail: List.Aux[A, P])
    extends List[A] {
  type N = Succ[P]
}

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
