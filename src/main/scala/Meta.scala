object Two extends Enumeration {
  type Ty = Value
  val X, Y = Value
}

object Three extends Enumeration {
  type Ty = Value
  val P, Q, R = Value
}

object Meta {

  import Three._
  import Two._
  import Terms._

  val (x, y) = (Var(X), Var(Y))

  val (p, q, r) = (Var(P), Var(Q), Var(R))

  def toTwo[T](x: T, y: T)(two: Two.Ty) = two match {
    case X => x
    case Y => y
  }

  def toThree[T](p: T, q: T, r: T)(three: Three.Ty) = three match {
    case P => p
    case Q => q
    case R => r
  }

}