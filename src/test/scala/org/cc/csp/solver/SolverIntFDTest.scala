package org.cc.csp.solver

import org.scalatest._

class SolverIntFDTest extends FunSuite {

  import Constraint._
  import ConstraintNetwork._
  import domain.fd.FD._
  import domain.fd.IntFD._
  import domain.fd.{IntFDConstraint => IFDC}


  val ac3FD = ac3[Int](IFDC.reduce) _
  val fdSolver = Solver.solve[Int](ac3FD)(IFDC.splitter) _

  test("Solver should be able to find when a trivial problem doesn't have solution - 1") {
    val variables = List("X","Y")
    val domains: Map[VarId,FD[Int]] = Map() +
      ("X"->set(1,2)) +
      ("Y" -> set(0,1))
    val constraints = List(LT("X","Y"))
    val cn = (variables, domains, constraints)

    assert(fdSolver(cn).isEmpty)
  }

  test("Solver should be able to find when a trivial problem doesn't have solution - 2") {
    val variables = List("X","Y","Z")
    val domains: Map[VarId,FD[Int]] = Map() +
      ("X" -> set(2,3)) +
      ("Y" -> set(2,3)) +
      ("Z" -> set(2,3))
    val constraints = List(LT("X","Y"),LT("Y","Z"))
    val cn = (variables, domains, constraints)

    assert(fdSolver(cn).isEmpty)
  }

  test("Solver should be able to find when a trivial problem doesn't have solution - 3") {
    val variables = List("W","X","Y","Z")
    val domains: Map[VarId,FD[Int]] =
      Map() +
        ("X" -> set(1,2,3,4)) +
        ("Y" -> set(2,3,4)) +
        ("Z" -> set(2,3,4)) +
        ("W" -> set(2,3))
    val constraints = List(LT("X","Y"), LT("Y","Z"), LT("Y","Z"), LT("Z","W"))
    val cn = (variables, domains, constraints)

    assert(fdSolver(cn).isEmpty)
  }

  test("Solver should be able to find when a trivial problem has a solution - 1") {
    val variables = List("X","Y")
    val domains: Map[VarId,FD[Int]] = Map() +
      ("X"->set(1,2)) +
      ("Y" -> set(0,1,2))
    val constraints = List(LT("X","Y"))
    val cn = (variables, domains, constraints)

    assert(1 === fdSolver(cn).size)
  }

  test("Solver should be able to find all the solutions of a trivial problem - 2") {
    val variables = List("V","W","X","Y","Z")
    val domains: Map[VarId,FD[Int]] = Map() +
      ("X" -> set(1,2,3,4,5)) +
      ("Y" -> set(1,2,3,4,5)) +
      ("Z" -> set(1,2,3,4,5)) +
      ("W" -> set(1,2,3,4,5)) +
      ("V" -> set(1,2,3,4,5))

    val constraints = List(
      LT("X","Y"),
      LT("Y","Z"),
      LT("Z","W"),
      LT("W","V"))
    val cn = (variables, domains, constraints)

    val solutions = fdSolver(cn)
    assert(1 === fdSolver(cn).size)

    // Checking solution
    val solution = solutions.head
    assert(constant(1) === solution("X"))
    assert(constant(2) === solution("Y"))
    assert(constant(3) === solution("Z"))
    assert(constant(4) === solution("W"))
    assert(constant(5) === solution("V"))
  }

  test("Solver should be able to find all the solutions of a trivial problem involving intervals - 1") {
    val variables = List("X","Y")
    val domains: Map[VarId,FD[Int]] = Map() +
      ("X" -> interval(1,3)) +
      ("Y" -> interval(2,4))

    val constraints = List(LT("X","Y"))
    val cn = (variables, domains, constraints)

    val solutions = fdSolver(cn)
    println(solutions.toList)
    assert(6 === fdSolver(cn).size)
    // FIXME solution (3,4) is missing
  }

}
