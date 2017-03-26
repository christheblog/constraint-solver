package org.cc.csp.solver

object Constraint {

  import collection.immutable.Seq

  type VarId = String

  trait Constraint {
    def variables: Seq[VarId]
    def count: Int
  }

  trait UnaryConstraint extends Constraint {
    require(variables.size==1)
    def count: Int = 1
  }

  trait BiConstraint extends Constraint {
    require(variables.size==2)
    def count: Int = 2
  }



  // Transformations

  def allDifferents(vs: VarId*) =
    for(v1 <- vs;v2 <- vs if v1!=v2) yield Different(v1, v2)


  // Constraints

  case class Equals(v1: VarId, v2: VarId) extends BiConstraint {
    def variables = Seq(v1, v2)
  }

  case class Different(v1: VarId,v2: VarId) extends BiConstraint {
    def variables = Seq(v1, v2)
  }


  // Constraints requiring some Ordering

  case class LT(v1: VarId,v2: VarId) extends BiConstraint {
    def variables = Seq(v1, v2)
  }

  case class LTE(v1: VarId,v2: VarId) extends BiConstraint {
    def variables = Seq(v1, v2)
  }

  case class GT(v1: VarId,v2: VarId) extends BiConstraint {
    def variables = Seq(v1, v2)
  }

  case class GTE(v1: VarId,v2: VarId) extends BiConstraint {
    def variables = Seq(v1, v2)
  }

}

