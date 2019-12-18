trait Semigroup[A] {
  def plus(a: A, b: A): A
}
object Semigroup {
  // If we have a Monoid[A], we also have a Semigroup[A]
  implicit def fromMonoid[A](implicit m: Monoid[A]): Semigroup[A] = m
  def apply[A](implicit s: Semigroup[A]): Semigroup[A] = s
}

trait Monoid[A] extends Semigroup[A]{
  def zero: A
}
object Monoid {
  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def zero: Int = 0
    def plus(a: Int, b: Int): Int = a+b
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def zero: List[A] = List.empty[A]
    def plus(a: List[A], b: List[A]): List[A] = a++b
  }

  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def zero: Option[A] = None
    def plus(a: Option[A], b: Option[A]): Option[A] =
      (a, b) match {
        case (None, None) => None
        case (Some(v), None) => Some(v)
        case (None, Some(v)) => Some(v)
        case (Some(v1), Some(v2)) => Some(Semigroup[A].plus(v1, v2))
      }
  }
}

object TypeClass {
  def add[A: Semigroup](a1: A, a2: A): A = Semigroup[A].plus(a1, a2)

  def main(args: Array[String]): Unit = {
    println("hello world")
    val ol1: Option[List[Int]] = Some(List(1, 2, 3))
    val ol2: Option[List[Int]] = Some(List(7))
    println(add(ol1, ol2))
    // println(add(1, 3))
  }
}
