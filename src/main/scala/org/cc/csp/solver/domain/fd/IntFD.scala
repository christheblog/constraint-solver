package org.cc.csp.solver.domain.fd


// Extending FD with Int values. We can add intervals
object IntFD {

  import FD._

  // An interval has inclusive bounds
  def interval(lower: Int, upper: Int): FD[Int] =
    normalize(Interval(lower,upper))

  protected[fd] def normalize(d: FD[Int]): FD[Int] = d match {
    case Interval(l,u) if u-l <= 0 => empty[Int]
    case Interval(l,u) if u==l => constant(u)
    case Interval(_,_) => d
    case _ => FD.normalize(d)
  }

  private[fd] case class Interval(lower: Int, upper: Int) extends FD[Int] {
    def contains(elt: Int) = elt >= lower && elt <= upper
    def enumerate = (lower to upper).toStream
    override def count = upper - lower + 1
  }

}
