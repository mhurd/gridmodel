package com.mhurd.gridmodel

import org.scalatest.FlatSpec
import scala.language.reflectiveCalls

class WrappingGridEqualitySpec extends FlatSpec {

  def emptyWrappingGridFixture =
    new {
      val grid1 = WrappingGrid[Int](3, 3, List())
      val grid2 = WrappingGrid[Int](3, 3, List())
      val grid3 = WrappingGrid[Int](4, 4, List())
    }

  def sameContentsWrappingGridFixture =
    new {
      val cells = List(
        ((1, 1), 1),
        ((1, 1), 1),
        ((1, 1), 1)
      )
      val grid1 = WrappingGrid[Int](3, 3, cells)
      val grid2 = WrappingGrid[Int](3, 3, cells)
      val grid3 = WrappingGrid[Int](4, 4, cells)
    }

  def differentContentsWrappingGridFixture =
    new {
      val cells1 = List(
        ((1, 1), 1),
        ((1, 1), 1),
        ((1, 1), 1)
      )
      val cells2 = List(
        ((1, 1), 1),
        ((1, 1), 2),
        ((1, 1), 3)
      )
      val grid1 = WrappingGrid[Int](3, 3, cells1)
      val grid2 = WrappingGrid[Int](3, 3, cells2)
      val grid3 = WrappingGrid[Int](4, 4, cells2)
    }

  def differentLocationsWrappingGridFixture =
    new {
      val cells1 = List(
        ((1, 1), 1),
        ((1, 1), 1),
        ((1, 1), 1)
      )
      val cells2 = List(
        ((1, 1), 1),
        ((1, 2), 1),
        ((2, 1), 1)
      )
      val grid1 = WrappingGrid[Int](3, 3, cells1)
      val grid2 = WrappingGrid[Int](3, 3, cells2)
      val grid3 = WrappingGrid[Int](4, 4, cells2)
    }

  "WrappingGridEquality" must "equal each other when empty" in {
    val f = emptyWrappingGridFixture
    expect(true) {
      f.grid1 == f.grid2
    }
  }

  it must "not equal a StandardGrid" in {
    expect(false) {
      WrappingGrid[Int](3, 3, List()) == StandardGrid[Int](3, 3, List())
    }
  }

  it must "have the same hashcode when empty" in {
    val f = emptyWrappingGridFixture
    expect(true) {
      f.grid1.hashCode() == f.grid2.hashCode()
    }
  }

  it must "not equal another empty grid of a different dimension" in {
    val f = emptyWrappingGridFixture
    expect(false) {
      f.grid2 == f.grid3
    }
  }

  it must "not have the same hashcode as another empty grid of a different dimension" in {
    val f = emptyWrappingGridFixture
    expect(false) {
      f.grid2.hashCode() == f.grid3.hashCode()
    }
  }

  it must "equal each other when containing the same contents in the same locations" in {
    val f = sameContentsWrappingGridFixture
    expect(true) {
      f.grid1 == f.grid2
    }
  }

  it must "have the same hashcode when containing the same contents in the same locations" in {
    val f = sameContentsWrappingGridFixture
    expect(true) {
      f.grid1.hashCode() == f.grid2.hashCode()
    }
  }

  it must "not equal each other when containing the same contents in the same locations but are grids of different sizes" in {
    val f = sameContentsWrappingGridFixture
    expect(false) {
      f.grid2 == f.grid3
    }
  }

  it must "not have the same hashcode when containing the same contents in the same locations but are grids of different sizes" in {
    val f = sameContentsWrappingGridFixture
    expect(false) {
      f.grid2.hashCode() == f.grid3.hashCode()
    }
  }

  it must "not equal another grid when containing different contents in the same locations" in {
    val f = differentContentsWrappingGridFixture
    expect(false) {
      f.grid1 == f.grid2
    }
  }

  it must "not have the same hashcode as another grid when containing different contents in the same locations" in {
    val f = differentContentsWrappingGridFixture
    expect(false) {
      f.grid1.hashCode() == f.grid2.hashCode()
    }
  }

  it must "not equal another grid when containing the same contents in different locations" in {
    val f = differentLocationsWrappingGridFixture
    expect(false) {
      f.grid1 == f.grid2
    }
  }

  it must "not have the same hashcode as another grid when containing the same contents in different locations" in {
    val f = differentLocationsWrappingGridFixture
    expect(false) {
      f.grid1.hashCode() == f.grid2.hashCode()
    }
  }

}