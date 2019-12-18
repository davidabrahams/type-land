trait Semigroup[A] {
  def plus(a: A, b: A): A
}
object Monoid {
  // If we have a Monoid[A], we also have a Semigroup[A]
  implicit def fromCanAddZ[A](implicit canAddZ: CanAddZ[A]): Semigroup[A] = canAddZ
  def apply[A](implicit canAdd: Semigroup[A]): Semigroup[A] = canAdd
}

trait CanAddZ[A] extends Semigroup[A]{
  def zero: A
}
object CanAddZ {
  implicit val canAddInts: CanAddZ[Int] = new CanAddZ[Int] {
    def zero: Int = 0
    def plus(a: Int, b: Int): Int = a+b
  }

  implicit def canAddLists[A]: CanAddZ[List[A]] = new CanAddZ[List[A]] {
    def zero: List[A] = List.empty[A]
    def plus(a: List[A], b: List[A]): List[A] = a++b
  }

  implicit def canAddOptions[A: Monoid]: CanAddZ[Option[A]] = new CanAddZ[Option[A]] {
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
  def add[A: Monoid](a1: A, a2: A) = Semigroup[A].plus(a1, a2)

  def main(args: Array[String]): Unit = {
    println("hello world")
    val ol1: Option[List[Int]] = Some(List(1, 2, 3))
    val ol2: Option[List[Int]] = Some(List(7))
    println(add(ol1, ol2))
    // println(add(1, 3))
  }
}
