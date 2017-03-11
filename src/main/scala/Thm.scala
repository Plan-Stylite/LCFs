
object Thm {

  import Axioms._
  import Terms._
  import Utils._
  import Props._

  val uu = ()

  val u = Var(uu)
  val x = Var(Two.X)
  val y = Var(Two.Y)

  def truthThm[T](p: Term[T]): Theorem[Unit] = {
    val s1 = inst2(u, rTruth(uu))(axiom1)
    val s2 = inst3(u, rTruth(uu), u)(axiom2)
    val s3 = mp(s2, s1)
    val s4 = inst2(u, u)(axiom1)
    s3.map(i => mp(i, s4)).get.get
  }

}
