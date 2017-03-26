package org.cc.csp.solver

import org.scalatest._

// Testing Arc-Consistency 3 algorithm
class AC3Test extends FunSuite {

  import Constraint._
  import domain.fd.FD._
  import domain.fd.{FDConstraint => FDC}
  import ConstraintNetwork._

  test("AC3 should return None when a 1-constraint network has no solution") {
    val ac3FD = ac3(FDC.reduce[Int]) _

    val variables = List("X","Y")
    val domains: Map[VarId,FD[Int]] = Map() + ("X" -> set(1,2,3,4)) + ("Y" -> set(10,20,30,40))
    val constraints = List(Equals("X","Y"))
    val cn = (variables, domains, constraints)

    assert(ac3FD(cn).isEmpty)
  }

  test("AC3 should return None when a 2-constraint network has no solution") {
    val ac3FD = ac3(FDC.reduce[Int]) _

    val variables = List("X","Y","Z")
    val domains: Map[VarId,FD[Int]] = Map() + ("X" -> set(1,2,3,4)) + ("Y" -> set(4,5)) + ("Z" -> set(5,6,7,8))
    val constraints = List(Equals("X","Y"),Equals("Y","Z"))
    val cn = (variables, domains, constraints)

    assert(ac3FD(cn).isEmpty)
  }

  test("AC3 should return None when a 3-constraint network has no solution") {
    val ac3FD = ac3(FDC.reduce[Int]) _

    val variables = List("W","X","Y","Z")
    val domains: Map[VarId,FD[Int]] =
      Map() + ("X" -> set(1,2,3,4,5)) + ("Y" -> set(4,5,6)) +
              ("Z" -> set(5,6,7,8)) + ("W" -> set(6,7,8))
    val constraints = List(Equals("X","Y"),Equals("Y","Z"),Equals("Z","W"))
    val cn = (variables, domains, constraints)

    assert(ac3FD(cn).isEmpty)
  }



  test("AC3 should reduce the domain of 2 variables when an arc is consistent in a 1-constraint network") {
    val ac3FD = ac3(FDC.reduce[Int]) _

    val variables = List("X","Y")
    val domains: Map[VarId,FD[Int]] = Map() + ("X" -> set(1,2,3,4)) + ("Y" -> set(4,5,6,7))
    val constraints = List(Equals("X","Y"))
    val cn = (variables, domains, constraints)

    val Some((_,newDomains,_)) = ac3FD(cn)
    val expected = Map("X" -> constant(4), "Y" -> constant(4))
    assert(expected === newDomains)
  }

  test("AC3 should reduce the domain of 3 variables when arcs are consistent in a 2-constraint network") {
    val ac3FD = ac3(FDC.reduce[Int]) _

    val variables = List("X","Y","Z")
    val domains: Map[VarId,FD[Int]] = Map() +
      ("X" -> set(1,2,3,4)) +
      ("Y" -> set(3,4,5)) +
      ("Z" -> set(4,5,6,7,8))
    val constraints = List(Equals("X","Y"),Equals("Y","Z"))
    val cn = (variables, domains, constraints)

    val Some((_,newDomains,_)) = ac3FD(cn)
    val expected = Map("X" -> constant(4), "Y" -> constant(4), "Z" -> constant(4))
    assert(expected === newDomains)
  }

  test("AC3 should reduce the domain of 4 variables when arcs are consistent in a 3-constraint network") {
    val ac3FD = ac3(FDC.reduce[Int]) _

    val variables = List("W","X","Y","Z")
    val domains: Map[VarId,FD[Int]] = Map() +
      ("X" -> set(1,2,3,4,5,6)) + ("Y" -> set(4,5,6)) +
      ("Z" -> set(4,5,6,7,8)) + ("W" -> set(6,7,8))
    val constraints = List(Equals("X","Y"),Equals("Y","Z"),Equals("Z","W"))
    val cn = (variables, domains, constraints)

    val Some((_,newDomains,_)) = ac3FD(cn)
    val expected = Map("X" -> constant(6), "Y" -> constant(6), "Z" -> constant(6), "W" -> constant(6))
    assert(expected === newDomains)
  }

  test("AC3 should reduce the domain of 4 variables when arcs are consistent in a 3-constraint network - 2") {
    val ac3FD = ac3(FDC.reduce[Int]) _

    val variables = List("W","X","Y","Z")
    val domains: Map[VarId,FD[Int]] = Map() +
      ("X" -> set(1,2,3,4,5,6)) + ("Y" -> set(4,5,6)) +
      ("Z" -> set(4,5,6,7,8)) + ("W" -> set(5,6,7,8))
    val constraints = List(Equals("X","Y"),Equals("Y","Z"),Equals("Z","W"))
    val cn = (variables, domains, constraints)

    val Some((_,newDomains,_)) = ac3FD(cn)
    val expected = Map("X" -> set(5,6), "Y" -> set(5,6), "Z" -> set(5,6), "W" -> set(5,6))
    assert(expected === newDomains)
  }

  test("AC3 should reduce the domain of 5 variables when arcs are consistent in a 4-constraint network") {
    val ac3FD = ac3(FDC.reduce[Int]) _

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

    val Some((_,newDomains,_)) = ac3FD(cn)
    val expected = Map("X" -> constant(6), "Y" -> constant(6), "Z" -> constant(6), "W" -> constant(6), "V" -> set(5,7,8))
    assert(expected === newDomains)
  }

}
