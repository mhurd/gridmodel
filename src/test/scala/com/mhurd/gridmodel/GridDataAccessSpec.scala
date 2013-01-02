package com.mhurd.gridmodel

import org.scalatest.FlatSpec

class GridDataAccessSpec extends FlatSpec {

  def emptyFixture =
    new {
      val grid = Grid[Int](3, 3)
    }

  def nonEmptyFixture =
    new {
      val grid = Grid[Int](3, 3)
      grid.put(0, 0, 1)
      grid.put(0, 1, 2)
      grid.put(1, 0, 3)
      grid.put(1, 1, 4)
      grid.put(1, 2, 5)
    }

  "GridDataAccess" must "be able to return empty cells if there is no data at a requested location" in {
    val f = emptyFixture
    expect(None) {
      f.grid.get(1, 1).content
    }
    expect(1) {
      f.grid.get(1, 1).x
    }
    expect(1) {
      f.grid.get(1, 1).y
    }
  }

  it must "be able to return the cell at a requested location" in {
    val f = nonEmptyFixture
    expect(Some(4)) {
      f.grid.get(1, 1).content
    }
  }

  it must "be able to return the cell at a requested location with the correct coords" in {
    val f = nonEmptyFixture
    expect(1) {
      f.grid.get(1, 1).x
    }
    expect(1) {
      f.grid.get(1, 1).y
    }
  }

  it must "be able to remove the cell at a requested location" in {
    val f = nonEmptyFixture
    f.grid.remove(1, 1)
    expect(None) {
      f.grid.get(1, 1).content
    }
  }

  it must "be able to add a cell at the requested location" in {
    val f = emptyFixture
    f.grid.put(1, 1, 36)
    expect(Some(36)) {
      f.grid.get(1, 1).content
    }
  }

  it must "be able to handle adding a cell to an out-of-bounds location (nullOp)" in {
      val f = emptyFixture
      f.grid.put(10, 10, 36)
      expect(true) {
        f.grid.get(10, 10).isOutOfBounds
      }
      expect(None) {
        f.grid.get(10, 10).content
      }
    }

  it must "be able to get the raw Map (immutable)" in {
    val f = nonEmptyFixture
    expect(5) {
      val theGrid: Map[(Int, Int), f.grid.GridCell] = f.grid.getGrid
      theGrid.size
    }
  }

  it must "be able to return a string representation of itself" in {
    val f = nonEmptyFixture
    expect("(1,0) = 3\n(1,2) = 5\n(1,1) = 4\n(0,1) = 2\n(0,0) = 1") {
      f.grid.toString
    }
  }

  it must "be able to return an ascii art representation of itself" in {
    val f = nonEmptyFixture
    expect("o * o \n* * o \n* * o \n") {
      f.grid.ascii()
    }
  }

  it must "be able to get the surrounding cells of any cell" in {
    val f = nonEmptyFixture
    expect("List((1,2) = 5, (2,2) = Empty, (2,1) = Empty, (2,0) = Empty, (1,0) = 3, (0,0) = 1, (0,1) = 2, (0,2) = Empty)") {
      f.grid.get(1, 1).surroundingCells.toString()
    }
  }

  it must "be able to get the surrounding cells of any cell - handling out of bounds cells" in {
    val f = nonEmptyFixture
    expect("List((0,1) = 2, (1,1) = 4, (1,0) = 3)") {
      f.grid.get(0, 0).surroundingCells.toString()
    }
  }

  it must "be able to say whether a cell is empty" in {
    val f = nonEmptyFixture
    expect(true) {
      f.grid.get(0, 2).isEmpty
    }
    expect(false) {
      f.grid.get(0, 0).isEmpty
    }
    expect("(0,2) = Empty") {
      f.grid.get(0, 2).toString
    }
  }

  it must "be able to say whether a cell is out of bounds" in {
    val f = nonEmptyFixture
    expect(true) {
      f.grid.get(0, -1).isOutOfBounds
    }
    expect(true) {
      f.grid.get(5, 5).isOutOfBounds
    }
    expect(false) {
      f.grid.get(0, 0).isOutOfBounds
    }
    expect("(5,5) = Out-of-bounds") {
      f.grid.get(5, 5).toString
    }
  }

  it must "be able to say access cells around a given cell using compass directions (north)" in {
    val f = nonEmptyFixture
    expect("(1,2) = 5") {
      f.grid.get(1, 1).north.toString
    }
  }

  it must "be able to say access cells around a given cell using compass directions (northEast)" in {
    val f = nonEmptyFixture
    expect("(2,2) = Empty") {
      f.grid.get(1, 1).northEast.toString
    }
  }

  it must "be able to say access cells around a given cell using compass directions (east)" in {
    val f = nonEmptyFixture
    expect("(2,1) = Empty") {
      f.grid.get(1, 1).east.toString
    }
  }

  it must "be able to say access cells around a given cell using compass directions (southEast)" in {
    val f = nonEmptyFixture
    expect("(2,0) = Empty") {
      f.grid.get(1, 1).southEast.toString
    }
  }

  it must "be able to say access cells around a given cell using compass directions (south)" in {
    val f = nonEmptyFixture
    expect("(1,0) = 3") {
      f.grid.get(1, 1).south.toString
    }
  }

  it must "be able to say access cells around a given cell using compass directions (southWest)" in {
    val f = nonEmptyFixture
    expect("(0,0) = 1") {
      f.grid.get(1, 1).southWest.toString
    }
  }

  it must "be able to say access cells around a given cell using compass directions (west)" in {
    val f = nonEmptyFixture
    expect("(0,1) = 2") {
      f.grid.get(1, 1).west.toString
    }
  }

  it must "be able to say access cells around a given cell using compass directions (northWest)" in {
    val f = nonEmptyFixture
    expect("(0,2) = Empty") {
      f.grid.get(1, 1).northWest.toString
    }
  }

}