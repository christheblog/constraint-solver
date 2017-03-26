package org.cc.csp.solver

import org.cc.csp.solver.domain.Domain.Domain
import org.cc.csp.solver.domain.fd.FD.FD


object Solver {

  import ConstraintNetwork._

  type Solution[A] = Domains[A]
  type AC[A] = ConstraintNetwork[A] => Option[ConstraintNetwork[A]]
  type Splitter[A] = Domain[A] => (Domain[A],Domain[A])

  def solve[A](ac: AC[A])(split: Splitter[A])(n: ConstraintNetwork[A]): Stream[Solution[A]] = {
    if(isSolution(n))
      Stream(domains(n))
    else {
      val reduced = ac(n)
      if(reduced.exists(isSolution))
        Stream(domains(reduced.get))
      else
        reduced
          .map(guess(split))
          .map { case (l, r) => solve(ac)(split)(l) ++ solve(ac)(split)(r) }
          .getOrElse(Stream.empty[Solution[A]])
    }
  }

  // Check the network is a solution
  def isSolution[A](n: ConstraintNetwork[A]) =
    domains(n).values.forall(_.hasSingleValue)

  // Try a guess on a variable and split the search space into a (graph with a guess C, graph with a not(C))
  // Note : we should add some variable selection strategy here ...
  def guess[A](split: Splitter[A])(n: ConstraintNetwork[A]): (ConstraintNetwork[A], ConstraintNetwork[A]) = {
    val (v,d,c) = n
    val (x,dx) = d.filter(_._2.count > 1).minBy(_._2.count)
    val (dx1,dx2) = split(dx)
    ((v,d + (x->dx1),c), (v,d + (x->dx2),c))
  }
}
