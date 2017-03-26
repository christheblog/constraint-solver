package org.cc.csp.solver

import org.cc.csp.solver.Constraint.Constraint

import scala.annotation.tailrec

object ConstraintNetwork {

  import org.cc.csp.solver.domain.Domain.Domain

  type VarId = String
  type Variables = List[VarId]            // X
  type Domains[A] = Map[VarId,Domain[A]]  // D
  type Constraints = List[Constraint]     // C
  // Constraint network = (X,D,C)
  type ConstraintNetwork[A] = (Variables, Domains[A], Constraints)

  // Binary constraints
  type Var[A] = (VarId, Domain[A])
  type ArcId= (VarId, VarId, Constraint)
  type Arc[A] = (Var[A], Var[A], Constraint)


  // CN functions

  def variables[A](n: ConstraintNetwork[A]) = n._1

  def domains[A](n: ConstraintNetwork[A]) = n._2

  def domainOf[A](n: ConstraintNetwork[A])(v: VarId) = n._2(v)

  def constraints[A](n: ConstraintNetwork[A]) = n._3

  def constraintFor[A](n: ConstraintNetwork[A])(v: VarId) =
    constraints(n).filter(c => c.variables.contains(v))

  def isSolution[A](n: ConstraintNetwork[A]) =
    domains(n).values.forall(d => d.hasSingleValue)

  def hasSolution[A](n: ConstraintNetwork[A]) =
    domains(n).values.forall(d => !d.isEmpty)


  // Arc consistency algorithm

  def toArcs[A](n: ConstraintNetwork[A]): List[Arc[A]] = {
    val bics = constraints(n).filter(_.count==2)
    bics.flatMap { bic =>
      val c1 +: c2 +: Seq() = bic.variables
      val arc1 = ((c1,domainOf(n)(c1)), (c2,domainOf(n)(c2)), bic)
      val arc2 = ((c2,domainOf(n)(c2)), (c1,domainOf(n)(c1)), bic)
      List(arc1,arc2)
    }
  }

  // We want arcs without the attached domains here
  def toArcIds[A](n: ConstraintNetwork[A]): List[ArcId] = {
    val bics = constraints(n).filter(_.count==2)
    bics.flatMap { bic =>
      val c1 +: c2 +: Seq() = bic.variables
      val arc1 = (c1, c2, bic)
      val arc2 = (c2, c1, bic)
      List(arc1,arc2)
    }
  }

  // https://en.wikipedia.org/wiki/AC-3_algorithm
  // Return an AC3 algorithm impl for the given domain
  def ac3[A](arcReduce: Arc[A] => Option[Domain[A]])
            (n: ConstraintNetwork[A]): Option[ConstraintNetwork[A]] = {

    // Unary constraints
    val unary = constraints(n).filter(_.count==1)
    // FIXME TODO :  make unary constraints consistent and reduce domains if possible

    // Binary constraints
    val all = toArcIds(n)
    def arcsEndingWith(arcs: List[ArcId])(end: VarId) =
      arcs.filter { case (_,name,_) => end==name }
    val endingWith = arcsEndingWith(all) _

    @tailrec
    def reduce(worklist: List[ArcId], domains: Domains[A]): Option[Domains[A]] = {
      worklist match {
        case Nil => Some(domains)
        case (x,y,c) :: cs =>
          val arc: Arc[A] = ((x,domains(x)), (y,domains(y)), c)
          arcReduce(arc) match {
            case None =>
              reduce(cs, domains)
            case Some(d1) if d1.isEmpty =>
              None
            case Some(d1) =>
              // Adding all arcs (_ -> x) that are not already in the worklist
              val newcs = cs ++ endingWith(x).filter(a => !worklist.contains(a))
              val newDomains = domains + (x -> d1)
              reduce(newcs, newDomains)
          }
      }
    }

    val reduced = reduce(all, domains(n))
    reduced.map(ds => (variables(n),ds,constraints(n)))
  }

}
