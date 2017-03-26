package org.cc.csp.solver.domain

object Domain {

  trait Domain[A] {
    def contains(elt: A): Boolean
    def count: Int
    def isEmpty = count ==0
    def hasSingleValue = count == 1
  }

  // Splits a domain in 2 non-overlapping parts
  type Splitter[A] = Domain[A] => (Domain[A], Domain[A])

}
