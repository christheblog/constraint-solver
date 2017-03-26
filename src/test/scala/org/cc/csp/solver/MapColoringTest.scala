package org.cc.csp.solver

import org.scalatest._

// Testing map colouring problem
class MapColoringTest extends FunSuite {

  import Constraint._
  import ConstraintNetwork._
  import domain.fd.FD._
  import domain.fd.{FDConstraint => FDC}

  // Colours
  sealed trait Colour
  case object Red extends Colour
  case object Green extends Colour
  case object Blue extends Colour
  case object Yellow extends Colour

  test("Solver should be able to solve map colouring problem when it has a solution - 1") {
// Example taken from here : http://www.cs.colostate.edu/~asa/courses/cs440/fall09/pdfs/10_csp.pdf
//    Schematic australia map
//
//                  |  Nothern        |
//                  |  Territory (NT) |  Queensland
//      Western     |---------------  |     (QL)
//      Australia   |  South          |-------------------
//        (WA)      |  Australia      |  New South Wales (NSW)
//                  |     (SA)        |-------------------
//                  |                 |  Victoria (V)

    val ac3FD = ac3(FDC.reduce[Colour]) _
    val fdSolver = Solver.solve(ac3FD)(FDC.splitter) _

    val variables = List("WA","NT","SA","QL","NSW","V")
    val colour: FD[Colour] = set(Red,Green,Blue)
    val domains: Map[VarId,FD[Colour]] =
      List("WA","NT","SA","QL","NSW","V").map(r => r -> colour).toMap

    val constraints = List(
      Different("WA","NT"),
      Different("WA","SA"),
      Different("NT","SA"),
      Different("NT","QL"),
      Different("SA","QL"),
      Different("SA","NSW"),
      Different("SA","V"),
      Different("NSW","QL"),
      Different("NSW","V")
    )
    val cn = (variables, domains, constraints)

    assert(fdSolver(cn).size==6)
  }


  test("Solver should be able to solve map colouring problem when it has a solution - 2") {
    // Example taken from here : http://www.cs.colostate.edu/~asa/courses/cs440/fall09/pdfs/10_csp.pdf
    // Forcing color of West Australia to Red
    //    Schematic australia map
    //
    //                  |  Nothern        |
    //                  |  Territory (NT) |  Queensland
    //      Western     |---------------  |     (QL)
    //      Australia   |  South          |-------------------
    //        (WA)      |  Australia      |  New South Wales (NSW)
    //   Forced to Red  |     (SA)        |-------------------
    //                  |                 |  Victoria (V)

    val ac3FD = ac3(FDC.reduce[Colour]) _
    val fdSolver = Solver.solve(ac3FD)(FDC.splitter) _

    val variables = List("WA","NT","SA","QL","NSW","V")
    val colour: FD[Colour] = set(Red,Green,Blue)
    val domains: Map[VarId,FD[Colour]] =
      (("WA" -> constant[Colour](Red)) +: List("NT","SA","QL","NSW","V").map(r => r -> colour)).toMap

    val constraints = List(
      Different("WA","NT"),
      Different("WA","SA"),
      Different("NT","SA"),
      Different("NT","QL"),
      Different("SA","QL"),
      Different("SA","NSW"),
      Different("SA","V"),
      Different("NSW","QL"),
      Different("NSW","V")
    )
    val cn = (variables, domains, constraints)

    assert(fdSolver(cn).size==2)
    // Checking that West Australia (WA) is Red in all the solutions
    assert(fdSolver(cn).forall(_("WA")==constant(Red)))
  }


}
