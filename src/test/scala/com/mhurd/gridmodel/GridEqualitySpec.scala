package com.mhurd.gridmodel

import org.scalatest.FlatSpec

class GridEqualitySpec extends FlatSpec {

  def emptyFixture =
    new {
      val grid1 = Grid[Int](2, 2)
      val grid2 = Grid[Int](2, 2)
      val grid3 = Grid[Int](2, 3)
    }

  def sameContentsFixture =
    new {
      val grid1 = Grid[Int](2, 2)
      val grid2 = Grid[Int](2, 2)
      val grid3 = Grid[Int](2, 3)
      grid1.put(1, 1, 1)
      grid2.put(1, 1, 1)
      grid3.put(1, 1, 1)
    }

  def differentContentsFixture =
    new {
      val grid1 = Grid[Int](2, 2)
      val grid2 = Grid[Int](2, 2)
      val grid3 = Grid[Int](2, 3)
      grid1.put(1, 1, 1)
      grid2.put(1, 1, 2)
      grid3.put(1, 1, 3)
    }

  def differentLocationsFixture =
    new {
      val grid1 = Grid[Int](2, 2)
      val grid2 = Grid[Int](2, 2)
      val grid3 = Grid[Int](2, 3)
      grid1.put(1, 1, 1)
      grid2.put(1, 2, 1)
      grid3.put(2, 1, 1)
    }

  "GridEquality" must "equal each other when empty" in {
    val f = emptyFixture
    expect(true) {
      f.grid1 == f.grid2
    }
  }

  it must "have the same hashcode when empty" in {
    val f = emptyFixture
    expect(true) {
      f.grid1.hashCode() == f.grid2.hashCode()
    }
  }

  it must "not equal another empty grid of a different dimension" in {
    val f = emptyFixture
    expect(false) {
      f.grid2 == f.grid3
    }
  }

  it must "not have the same hashcode as another empty grid of a different dimension" in {
    val f = emptyFixture
    expect(false) {
      f.grid2.hashCode() == f.grid3.hashCode()
    }
  }

  it must "equal each other when containing the same contents in the same locations" in {
    val f = sameContentsFixture
    expect(true) {
      f.grid1 == f.grid2
    }
  }

  it must "have the same hashcode when containing the same contents in the same locations" in {
    val f = sameContentsFixture
    expect(true) {
      f.grid1.hashCode() == f.grid2.hashCode()
    }
  }

  it must "not equal each other when containing the same contents in the same locations but are grids of different sizes" in {
    val f = sameContentsFixture
    expect(false) {
      f.grid2 == f.grid3
    }
  }

  it must "not have the same hashcode when containing the same contents in the same locations but are grids of different sizes" in {
    val f = sameContentsFixture
    expect(false) {
      f.grid2.hashCode() == f.grid3.hashCode()
    }
  }

  it must "not equal another grid when containing different contents in the same locations" in {
    val f = differentContentsFixture
    expect(false) {
      f.grid1 == f.grid2
    }
  }

  it must "not have the same hashcode as another grid when containing different contents in the same locations" in {
    val f = differentContentsFixture
    expect(false) {
      f.grid1.hashCode() == f.grid2.hashCode()
    }
  }

  it must "not equal another grid when containing the same contents in different locations" in {
    val f = differentLocationsFixture
    expect(false) {
      f.grid1 == f.grid2
    }
  }

  it must "not have the same hashcode as another grid when containing the same contents in different locations" in {
    val f = differentLocationsFixture
    expect(false) {
      f.grid1.hashCode() == f.grid2.hashCode()
    }
  }

}