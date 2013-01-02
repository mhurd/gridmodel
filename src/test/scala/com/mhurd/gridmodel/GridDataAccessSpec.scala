package com.mhurd.gridmodel

import org.scalatest.FlatSpec

class GridDataAccessSpec extends FlatSpec {

  def nonEmptyFixture =
    new {
      val cells = List(
        (0, 0, 1),
        (0, 1, 2),
        (1, 0, 3),
        (1, 1, 4),
        (1, 2, 5)
      )
      val grid = Grid[Int](3, 3, cells)
    }

  def nonEmptyOutOfBoundsFixture =
    new {
      val cells = List(
        (0, 0, 1),
        (0, 1, 2),
        (1, 0, 3),
        (1, 1, 4),
        (1, 2, 5),
        (10, 10, 36)
      )
      val grid = Grid[Int](3, 3, cells)
    }

  "GridDataAccess" must "be able to be constructed using a list of tuples (handling out out-of-bounds cells)" in {
    val f = nonEmptyOutOfBoundsFixture
    expect(true) {
      f.grid.get(10, 10).isOutOfBounds
    }
    expect(None) {
      f.grid.get(10, 10).content
    }
  }

  it must "be able to be constructed using a list of tuples " in {
    val f = nonEmptyFixture
    expect(Some(5)) {
      f.grid.get(1, 2).content
    }
  }

  it must "be able to return empty cells if there is no data at a requested location" in {
    val f = nonEmptyFixture
    expect(None) {
      f.grid.get(2, 2).content
    }
    expect(2) {
      f.grid.get(2, 2).x
    }
    expect(2) {
      f.grid.get(2, 2).y
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

  it must "be able to get the raw Map of non-empty cells (immutable)" in {
    val f = nonEmptyFixture
    expect(5) {
      val theGrid: Map[(Int, Int), Cell[Int]] = f.grid.cellMap
      theGrid.size
    }
  }

  it must "be able to get the cells as a sorted list, including empty cells (immutable)" in {
    val f = nonEmptyFixture
    expect(9) {
      val theCells: List[Cell[Int]] = f.grid.cellList
      theCells.size
    }
  }

  it must "be able to return a string representation of itself" in {
    val f = nonEmptyFixture
    expect("(1,1) = 4\n(1,0) = 3\n(1,2) = 5\n(0,0) = 1\n(0,1) = 2") {
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
    expect("List((0,0) = 1, (0,1) = 2, (0,2) = Empty, (1,0) = 3, (1,2) = 5, (2,0) = Empty, (2,1) = Empty, (2,2) = Empty)") {
      f.grid.get(1, 1).surroundingCells.toString()
    }
  }

  it must "be able to get the surrounding cells of any cell - handling out of bounds cells" in {
    val f = nonEmptyFixture
    expect("List((0,1) = 2, (1,0) = 3, (1,1) = 4)") {
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

  it must "be able to access cells around a given cell using compass directions (north)" in {
    val f = nonEmptyFixture
    expect("(1,2) = 5") {
      f.grid.get(1, 1).north.toString
    }
  }

  it must "be able to access cells around a given cell using compass directions (northEast)" in {
    val f = nonEmptyFixture
    expect("(2,2) = Empty") {
      f.grid.get(1, 1).northEast.toString
    }
  }

  it must "be able to access cells around a given cell using compass directions (east)" in {
    val f = nonEmptyFixture
    expect("(2,1) = Empty") {
      f.grid.get(1, 1).east.toString
    }
  }

  it must "be able to access cells around a given cell using compass directions (southEast)" in {
    val f = nonEmptyFixture
    expect("(2,0) = Empty") {
      f.grid.get(1, 1).southEast.toString
    }
  }

  it must "be able to access cells around a given cell using compass directions (south)" in {
    val f = nonEmptyFixture
    expect("(1,0) = 3") {
      f.grid.get(1, 1).south.toString
    }
  }

  it must "be able to access cells around a given cell using compass directions (southWest)" in {
    val f = nonEmptyFixture
    expect("(0,0) = 1") {
      f.grid.get(1, 1).southWest.toString
    }
  }

  it must "be able to access cells around a given cell using compass directions (west)" in {
    val f = nonEmptyFixture
    expect("(0,1) = 2") {
      f.grid.get(1, 1).west.toString
    }
  }

  it must "be able to access cells around a given cell using compass directions (northWest)" in {
    val f = nonEmptyFixture
    expect("(0,2) = Empty") {
      f.grid.get(1, 1).northWest.toString
    }
  }

}