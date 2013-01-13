package com.mhurd.gridmodel

import org.scalatest.FlatSpec
import scala.language.reflectiveCalls

class StandardGridSpec extends FlatSpec {

  def nonEmptyFixture =
    new {
      val cells = List(
        ((0, 0), 1),
        ((0, 1), 2),
        ((1, 0), 3),
        ((1, 1), 4),
        ((1, 2), 5)
      )
      val grid = StandardGrid[Int](3, 3, cells)
    }

  "StandardGrid" must "be able to get the surrounding cells of any cell" in {
    val f = nonEmptyFixture
    expect("List((0,0) = 1, (0,1) = 2, (0,2) = Empty, (1,0) = 3, (1,2) = 5, (2,0) = Empty, (2,1) = Empty, (2,2) = Empty)") {
      f.grid.surroundingCellsOf(1, 1).toString()
    }
  }

  it must "be able to get the surrounding cells of any cell - handling out of bounds cells" in {
    val f = nonEmptyFixture
    expect("List((0,1) = 2, (1,0) = 3, (1,1) = 4)") {
      f.grid.surroundingCellsOf(0, 0).toString()
    }
  }

  it must "be able to access cells around a given cell using compass directions (north)" in {
    val f = nonEmptyFixture
    expect("(1,2) = 5") {
      f.grid.northOf(1, 1).toString()
    }
  }

  it must "be able to access cells around a given cell using compass directions (northEast)" in {
    val f = nonEmptyFixture
    expect("(2,2) = Empty") {
      f.grid.northEastOf(1, 1).toString()
    }
  }

  it must "be able to access cells around a given cell using compass directions (east)" in {
    val f = nonEmptyFixture
    expect("(2,1) = Empty") {
      f.grid.eastOf(1, 1).toString()
    }
  }

  it must "be able to access cells around a given cell using compass directions (southEast)" in {
    val f = nonEmptyFixture
    expect("(2,0) = Empty") {
      f.grid.southEastOf(1, 1).toString()
    }
  }

  it must "be able to access cells around a given cell using compass directions (south)" in {
    val f = nonEmptyFixture
    expect("(1,0) = 3") {
      f.grid.southOf(1, 1).toString()
    }
  }

  it must "be able to access cells around a given cell using compass directions (southWest)" in {
    val f = nonEmptyFixture
    expect("(0,0) = 1") {
      f.grid.southWestOf(1, 1).toString()
    }
  }

  it must "be able to access cells around a given cell using compass directions (west)" in {
    val f = nonEmptyFixture
    expect("(0,1) = 2") {
      f.grid.westOf(1, 1).toString()
    }
  }

  it must "be able to access cells around a given cell using compass directions (northWest)" in {
    val f = nonEmptyFixture
    expect("(0,2) = Empty") {
      f.grid.northWestOf(1, 1).toString()
    }
  }

}