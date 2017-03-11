

object Utils {

  import Meta._
  import Props._
  import Terms._

  def inst1[T](p: Term[T], tu: Theorem[Unit]): Theorem[T] = inst(const(p), tu)

  def inst2[T](x: Term[T], y: Term[T])(two: Theorem[Two.Ty]) = inst(toTwo(x, y), two)

  def inst3[T](p: Term[T], q: Term[T], r: Term[T])(three: Theorem[Three.Ty]) = inst(toThree(p, q, r), three)

  def rDestAnd[T](t: Term[T]): (Term[T], Term[T]) = t match {
    // partial function
    case (~(->>(pi, ~(qi)))) => (pi, qi)
  }

  // https://github.com/Chattered/proplcf/blob/master/Utils.hs#L35
  def rDestEq[T](pq: Term[T]): (Term[T], Term[T]) = {
    // TODO deal with partial function
    rDestAnd(pq) match {
      case (pi ->> qi, _) => (pi, qi)
    }
  }

  def rTruth[T](a: T): Term[T] = Var(a) ->> Var(a)

  def rFalse[T](a: T): Term[T] = ~rTruth(a)

}
