package com.mhurd.gridmodel

import org.scalatest.FlatSpec
import scala.language.reflectiveCalls

class CovariantGridSpec extends FlatSpec {

  class A {
    override def toString: String = "A"
  }

  class B extends A {
    override def toString: String = "B"
  }

  def nonEmptyFixture =
    new {
      val cells = List(
        ((0, 0), new B()),
        ((0, 1), new B()),
        ((1, 0), new B()),
        ((1, 1), new B()),
        ((1, 2), new B())
      )
      val grid = StandardGrid[B](3, 3, cells)
    }

  "CovariantGridSpec" must "be able to behave as a covariant type" in {
    val f = nonEmptyFixture
    val grid: Grid[A] = f.grid.add((1, 1), new A())
    expect("(1,1) = A") {
      grid.get((1, 1)).toString
    }
    expect("(0,1) = B") {
      grid.get((0, 1)).toString
    }
  }

}