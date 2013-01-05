package com.mhurd.gridmodel

import org.scalatest.FlatSpec

class AbstractGridSpec extends FlatSpec {

  def nonEmptyFixture =
    new {
      val cells = List(
        (0, 0, 1),
        (0, 1, 2),
        (1, 0, 3),
        (1, 1, 4),
        (1, 2, 5)
      )
      val grid = StandardGrid[Int](3, 3, cells)
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
      val grid = StandardGrid[Int](3, 3, cells)
    }

  def conwayFixture =
    new {
      val cells = List(
        (0, 0, 1),
        (2, 1, 1),
        (1, 2, 1),
        (2, 2, 1),
        (3, 2, 1),
        (2, 3, 1)
      )
      val grid = StandardGrid[Int](4, 4, cells)
    }

  "GridDataAccess" must "be able to be constructed using a list of tuples representing the desired cells (handling out out-of-bounds cells)" in {
    val f = nonEmptyOutOfBoundsFixture
    expect(true) {
      f.grid.get(10, 10).isOutOfBounds
    }
    expect(None) {
      f.grid.get(10, 10).content
    }
  }

  it must "be able to be constructed using a list of tuples representing the desired cells" in {
    val f = nonEmptyFixture
    expect(5) {
      f.grid.cellMap.size
    }
    expect(Some(1)) {
      f.grid.get(0, 0).content
    }
    expect(Some(2)) {
      f.grid.get(0, 1).content
    }
    expect(Some(3)) {
      f.grid.get(1, 0).content
    }
    expect(Some(4)) {
      f.grid.get(1, 1).content
    }
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
    expect("(0,0) = 1\n(1,1) = 4\n(0,1) = 2\n(1,2) = 5\n(1,0) = 3") {
      f.grid.toString
    }
  }

  it must "be able to return an ascii art representation of itself" in {
    val f = nonEmptyFixture
    expect("o x o \nx x o \nx x o \n") {
      f.grid.ascii()
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

  it must "be able to add a new cell returning a new grid containing the old cells plus the new one" in {
    val f = nonEmptyFixture
    expect("(0,2) = 1\n(0,0) = 1\n(1,1) = 4\n(0,1) = 2\n(1,2) = 5\n(1,0) = 3") {
      f.grid.add((0, 2, 1)).toString
    }
  }

  it must "be able to transform the grid into a new grid" in {
    val f = nonEmptyFixture
    expect("(0,0) = 2\n(1,1) = 8\n(0,1) = 4\n(1,2) = 10\n(1,0) = 6") {
      f.grid.transform(
        (cell) => cell.content match {
          case Some(c) => Some(c * 2)
          case None => None
        }
      ).toString
    }
  }

  it must "be able to transform the grid into a new grid using conway's game of life rules" in {
    val f = conwayFixture
    expect("(3,1) = 1\n(3,2) = 1\n(1,3) = 1\n(3,3) = 1\n(2,3) = 1\n(1,2) = 1\n(2,1) = 1") {
      f.grid.transform(
        (cell) => {
          val occupiedNeighbors = cell.surroundingCells.filter(c => !c.isEmpty)
          cell.content match {
            case Some(c) => {
              if (occupiedNeighbors.size == 2 || occupiedNeighbors.size == 3) Some(c)
              else None
            }
            case None => {
              if (occupiedNeighbors.size == 3) Some(1)
              else None
            }
          }
        }
      ).toString
    }
  }

}