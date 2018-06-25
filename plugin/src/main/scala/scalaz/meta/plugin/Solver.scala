package scalaz.meta.plugin

class Solver[Literal] {
  type Clause  = List[Literal] // conjoined
  type Formula = List[Clause] // disjoined
  type Axioms  = Set[Literal]

  def isClauseSatisfied(axioms: Axioms)(clause: Clause): Option[Clause] =
    clause.filterNot(axioms.contains) match {
      case Nil   => None
      case unsat => Some(unsat)
    }

  def isFormulaSatisfied(axioms: Axioms)(formula: Formula): Option[Formula] = {
    val satisfictions = formula.map(isClauseSatisfied(axioms))
    if (satisfictions contains None) None
    else Some(satisfictions.flatten)
  }

  def showClause(clause: Clause): String = clause match {
    case only :: Nil => only.toString
    case _           => clause.mkString("(", " AND ", ")")
  }

  def showFormula(formula: Formula): String =
    formula.map(showClause).mkString(" OR ")
}
