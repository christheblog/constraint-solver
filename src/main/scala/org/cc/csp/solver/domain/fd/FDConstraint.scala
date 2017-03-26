package org.cc.csp.solver.domain.fd

import org.cc.csp.solver.domain.Domain.Domain


object FDConstraint {

  import org.cc.csp.solver.Constraint._
  import FD._

  type Var[A] = (VarId, Domain[A])
  type Arc[A] = (Var[A], Var[A], Constraint)
  type Domains[A] = Map[VarId,Domain[A]]


  // Make the domain dx consistent with arc (x --c--> y)
  //  None means no reduction has been done
  //  Some(_) means a change in the domain dx has been done
  def reduce[A](arc: Arc[A]): Option[Domain[A]] = {
    val ((_,dx: FD[A]), (_,dy: FD[A]), constraint) = arc
    val newdx = constraint match {
      case Equals(_,_) => intersect(dx, dy)
      case Different(_,_) =>
        (dx, dy) match {
          case (_,Cst(_)) => difference(dx, dy)
          case _ => dx
        }
      case unhandled =>
        throw new RuntimeException(s"Constraint ${unhandled} is not handled by this implementation")
    }
    if(newdx==dx) None else Some(newdx)
  }

  // Default splitter for a FD domain
  def splitter[A](d: Domain[A]): (Domain[A], Domain[A]) = d match {
    case Empty() => (Empty(),Empty())
    case Cst(x) => (Cst(x),Empty())
    case CSet(values) =>
      val (head,tail) = (values.head, values.tail)
      (Cst(head), normalize(CSet(tail)))
    case unhandled =>
      throw new RuntimeException(s"Domain ${unhandled} is not handled by this splitting function")
  }

}
