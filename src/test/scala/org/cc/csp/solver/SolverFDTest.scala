package org.cc.csp.solver

import org.scalatest._

class SolverFDTest extends FunSuite {

  import Constraint._
  import domain.fd.FD._
  import domain.fd.{FDConstraint => FDC}
  import ConstraintNetwork._

  val ac3FD = ac3(FDC.reduce[Int]) _
  val fdSolver = Solver.solve(ac3FD)(FDC.splitter) _

  test("Solver should be able to find when a trivial problem doesn't have solution - 1") {
    val variables = List("X","Y")
    val domains: Map[VarId,FD[Int]] = Map() + ("X" -> set(1,2,3,4)) + ("Y" -> set(10,20,30,40))
    val constraints = List(Equals("X","Y"))
    val cn = (variables, domains, constraints)

    assert(fdSolver(cn).isEmpty)
  }

  test("Solver should be able to find when a trivial problem doesn't have solution - 2") {
    val variables = List("X","Y","Z")
    val domains: Map[VarId,FD[Int]] = Map() + ("X" -> set(1,2,3,4)) + ("Y" -> set(4,5)) + ("Z" -> set(5,6,7,8))
    val constraints = List(Equals("X","Y"),Equals("Y","Z"))
    val cn = (variables, domains, constraints)

    assert(fdSolver(cn).isEmpty)
  }

  test("Solver should be able to find when a trivial problem doesn't have solution - 3") {
    val variables = List("W","X","Y","Z")
    val domains: Map[VarId,FD[Int]] =
      Map() + ("X" -> set(1,2,3,4,5)) + ("Y" -> set(4,5,6)) +
        ("Z" -> set(5,6,7,8)) + ("W" -> set(6,7,8))
    val constraints = List(Equals("X","Y"),Equals("Y","Z"),Equals("Z","W"))
    val cn = (variables, domains, constraints)

    assert(fdSolver(cn).isEmpty)
  }


  test("Solver should be able to find all the solutions of a trivial problem") {
    val variables = List("V","W","X","Y","Z")
    val domains: Map[VarId,FD[Int]] = Map() +
      ("X" -> set(1,2,3,4,5,6)) + ("Y" -> set(4,5,6)) +
      ("Z" -> set(4,5,6,7,8)) + ("W" -> set(6,7,8)) +
      ("V" -> set(5,6,7,8))

    val constraints = List(
      Equals("X","Y"),
      Equals("Y","Z"),
      Equals("Z","W"),
      Different("V","W"))
    val cn = (variables, domains, constraints)

    assert(fdSolver(cn).size==3)
  }

}
