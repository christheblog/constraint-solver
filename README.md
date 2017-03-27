# As small constraint-solver implementation


Trying to build a constraint-solver able to solve classic constraint-solving problem.

_Note : This is an on-going work, and the design is still not final.
Idea would be to reach a design where one can plug a domain (custom or not) and a consistency algorithm (custom or not) into a generic problem solver._

## Arc Consistency : AC3

At the moment, Arc Consistency is handled with an implementation of AC3 algorithm.

## Domains

The current implementation support only Finite Domains (FD) - and only the constraints Equals and Different.
This is still allowing to solve a toy map-coloring problem. Next step will be the N-Queens.

## Map coloring problem

The constraint solver can be used in the following way :

```scala
  
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
  
    sealed trait Colour
    case object Red extends Colour
    case object Green extends Colour
    case object Blue extends Colour
    
    // AC3 for the Colour domain
    val ac3FD = ac3(FDC.reduce[Colour]) _
    // Solver instanciated with AC3 for arc consistency
    val fdSolver = Solver.solve(ac3FD)(FDC.splitter) _
  
    // Problem description
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
    
    // Constraint network : (X,D,C)
    val cn = (variables, domains, constraints)
    val solutions: Stream[Solution] = fdSolver(cn)  
```