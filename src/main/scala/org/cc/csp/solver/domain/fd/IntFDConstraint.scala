package org.cc.csp.solver.domain.fd

object IntFDConstraint {

  import org.cc.csp.solver.domain.Domain.Domain
  import org.cc.csp.solver.domain.fd.FD.FD
  import org.cc.csp.solver.Constraint._
  import IntFD._

  type Var[A] = (VarId, Domain[A])
  type Arc[A] = (Var[A], Var[A], Constraint)
  type Domains[A] = Map[VarId,Domain[A]]


  // Make the domain dx consistent with arc (x --c--> y)
  //  None means no reduction has been done
  //  Some(_) means a change in the domain dx has been done - and wraps the new dx
  def reduce(arc: Arc[Int]): Option[Domain[Int]] = {
    val ((x,dx: FD[Int]), (y,dy: FD[Int]), constraint) = arc
    val newdx = constraint match {
      case LT(vx,_) if vx==y =>
        val reversed = ((x,dx), (y,dy), GT(x,y))
        reduce(reversed).getOrElse(dx)
      case LT(_,_) =>
        (dx,dy) match {
          case (Interval(l1,u1), Interval(l2,u2)) =>
            if(l1 > u2) FD.empty[Int]
            if(u1 >= u2) interval(l1,u2-1)
            else interval(l1,u1)
          case _ =>
            FD.set((for(elt1 <- dx.enumerate;elt2 <- dy.enumerate if elt1 < elt2) yield elt1).distinct:_*)
        }
      case GT(vx,_) if vx==y =>
        val reversed =  ((x,dx), (y,dy), LT(x,y))
        reduce(reversed).getOrElse(dx)
      case GT(vx,_) =>
        (dx,dy) match {
          case (Interval(l1,u1), Interval(l2,u2)) =>
            // To review
            if(l1 < u2) FD.empty[Int]
            if(u1 <= u2) interval(l1,u2-1)
            else interval(l1,u1)
          case _ =>
            FD.set((for(elt1 <- dx.enumerate;elt2 <- dy.enumerate if elt1 > elt2) yield elt1).distinct:_*)
        }
      case _ =>
        FDConstraint.reduce(arc).getOrElse(dx)
    }
    if(newdx==dx) None else Some(newdx)
  }

  // Default splitter for a IntFD domain
  def splitter(d: Domain[Int]): (Domain[Int], Domain[Int]) = d match {
    case Interval(l,u) => (FD.constant(l),interval(l+1,u))
    case _ => FDConstraint.splitter(d)
  }

}
