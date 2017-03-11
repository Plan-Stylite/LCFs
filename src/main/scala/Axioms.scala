
object Axioms {

  import Meta._
  import Props._
  import Terms._

  // The K combinator.
  def axiom1: Theorem[Two.Ty] = Theorem {
    x ->> (y ->> x)
  }

  // The S combinator.
  def axiom2: Theorem[Three.Ty] = Theorem {
    (p ->> (q ->> r)) ->> ((p ->> q) ->> (p ->> r))
  }

  // The classical axiom.
  def axiom3: Theorem[Two.Ty] = Theorem {
    (~x ->> ~y) ->> (y ->> x)
  }

  // Modus Ponens.
  def mp[T](pqImp: Theorem[T], pp: Theorem[T]) = (pqImp, pp) match {
    case (Theorem(pT ->> qT), Theorem(pPT)) if pT == pPT => Option(Theorem(qT))
    case _ => None
  }

}
