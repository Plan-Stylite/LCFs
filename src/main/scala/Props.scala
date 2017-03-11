

object Terms {

  sealed abstract class Term[T] {
    def \/(that: Term[T]): Term[T] = (~this) ->> that

    def /\(that: Term[T]): Term[T] = ~(this ->> (~that))

    def <=>(that: Term[T]): Term[T] = (this ->> that) /\ (that ->> this)
  }

  final case class Var[T](a: T) extends Term[T]

  final case class ->>[T](t1: Term[T], t2: Term[T]) extends Term[T]

  final case class ~[T](t: Term[T]) extends Term[T]

  implicit class TermOps[T](t: Term[T]) {
    def ->>(o: Term[T]) = new ->>(t, o)

    def unary_~ = new ~(t)
  }

}


object Props {

  case class Theorem[T](t: Terms.Term[T])

  import Terms._

  def const[A, B](a: A)(b: B): A = a

  def instTerm[A, B](f: A => Term[B])(at: Term[A]): Term[B] = at match {
    case Var(x) => f(x)
    case ~(t) => ~instTerm(f)(t)
    case a ->> c => instTerm(f)(a) ->> instTerm(f)(c)
  }

  def inst[A, B](f: A => Term[B], a: Theorem[A]): Theorem[B] = Theorem(instTerm(f)(a.t))

}