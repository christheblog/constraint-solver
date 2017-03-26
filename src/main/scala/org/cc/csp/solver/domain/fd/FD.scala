package org.cc.csp.solver.domain.fd

object FD {

  import org.cc.csp.solver.domain.Domain._

  // Finite domain => we can enumerate elements
  trait FD[A] extends Domain[A] {
    def enumerate: Stream[A]
    // A solution is a domain with a single value
    def count = enumerate.size
  }


  // Smart constructors

  def empty[A]: FD[A] = Empty[A]()

  def constant[A](value: A): FD[A] =
    Cst(value)

  def set[A](values: A*): FD[A] =
    normalize(CSet(values.toSet))

  protected[fd] def normalize[A](d: FD[A]) = d match {
    case CSet(_) =>
      if(d.isEmpty) empty[A]
      else if(d.count==1) constant(d.enumerate.head)
      else d
    case _ => d
  }

  // Intersect 2 domains
  def intersect[A](d1: FD[A], d2: FD[A]): FD[A] =
    normalize(set((d1.enumerate intersect d2.enumerate):_*))


  def union[A](d1: FD[A], d2: FD[A]): FD[A] =
    normalize(set((d1.enumerate union d2.enumerate):_*))


  def difference[A](d1: FD[A], d2: FD[A]): FD[A] = {
    val d2s = d2.enumerate.toSet
    val res = d1.enumerate.filter(!d2s(_))
    normalize(set(res:_*))
  }

  //

  private[fd] case class Empty[A]() extends FD[A] {
    def contains(elt: A) = false
    def enumerate = Stream.empty
    override def count = 0
  }

  private[fd] case class Cst[A](value: A) extends FD[A] {
    def contains(elt: A) = value==elt
    def enumerate = Stream(value)
    override def count = 1
  }

  private[fd] case class CSet[A](values: Set[A]) extends FD[A] {
    //require(!values.isEmpty)
    def contains(elt: A) = values(elt)
    def enumerate = values.toStream
    override def count = values.size
  }

}

