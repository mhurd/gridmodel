package com.mhurd.gridmodel

import collection.mutable.ListBuffer
import scala.Some
import language.postfixOps

sealed case class Cell[+T](coord: Coord, contents: Option[T]) {

  def x: Int = coord._1

  def y: Int = coord._2

  override def toString: String = {
    val base = "(" + x + "," + y + ") = "
    if (contents.isEmpty) base + "Empty"
    else base + contents.get
  }

  override def equals(obj: Any) = obj match {
    case that: Cell[T] => {
      (this eq that) || (canEqual(that) && this.x == that.x && this.y == that.y && this.contents == that.contents)
    }
    case _ => false
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Cell[T]]

  override def hashCode(): Int = {
    41 * (
      41 * (
        41 + x.hashCode()
        ) + y.hashCode()
      ) + contents.hashCode()
  }

}

/**
 * Represents the grid itself
 * @tparam T the type of objects the grid cells contain
 */
sealed trait Grid[+T] {

  def width: Int

  def height: Int

  def cellMap: Map[Coord, Cell[T]]

  def cellList: List[Cell[T]]

  def get(coord: Coord): Cell[T]

  def transform[U >: T](f: (Cell[U]) => Option[U]): Grid[U]

  def ascii: String

  def add[U >: T](cell: (Coord, U)): Grid[U]

  def isOutOfBounds(coord: Coord): Boolean

  def surroundingCellsOf(coord: Coord): List[Cell[T]]

  def northOf(coord: Coord): Cell[T]

  def northEastOf(coord: Coord): Cell[T]

  def eastOf(coord: Coord): Cell[T]

  def southEastOf(coord: Coord): Cell[T]

  def southOf(coord: Coord): Cell[T]

  def southWestOf(coord: Coord): Cell[T]

  def westOf(coord: Coord): Cell[T]

  def northWestOf(coord: Coord): Cell[T]

}

/**
 * The abstract grid implementation.
 *
 * @param width of the grid (in cells)
 * @param height of the grid (in cells)
 * @param initialCells the initial grid contents
 * @tparam T the type of objects the grid cells contain
 */
sealed abstract class AbstractGrid[T](width: Int, height: Int, initialCells: List[(Coord, T)]) extends Grid[T] {

  implicit object CellOrdering extends Ordering[Cell[T]] {
    def compare(a: Cell[T], b: Cell[T]) = {
      val xComp = a.x compare b.x
      if (xComp == 0) a.y compare b.y
      else xComp
    }
  }

  def isOutOfBounds(coord: Coord): Boolean = {
    coord match {
      case (x, y) => x >= width || y >= height || x < 0 || y < 0
      case _ => throw new IllegalArgumentException(coord + " is not a coordinate (Int, Int)!")
    }
  }

  def surroundingCellsOf(coord: Coord): List[Cell[T]] = {
    val b = ListBuffer[Cell[T]]()
    northOf(coord) +=:
      northEastOf(coord) +=:
      eastOf(coord) +=:
      southEastOf(coord) +=:
      southOf(coord) +=:
      southWestOf(coord) +=:
      westOf(coord) +=:
      northWestOf(coord) +=:
      b
    (b.toList filter (cell => !isOutOfBounds(cell.coord))).sorted
  }

  /*
   Holds the map of non-empty cells
   */
  val cellMap: Map[Coord, Cell[T]] = (for {
    (coord, contents) <- initialCells
    if (!isOutOfBounds(coord))
  } yield (coord -> Cell(coord, Option(contents)))).toMap

  /*
   Gets an ordered list of all the cells that make up the grid, including empty cells
   which are constructed on the fly
   */
  val cellList: List[Cell[T]] = {
    (for {
      y <- ((0 until height) reverse)
      x <- 0 until width
    } yield {
      get(x, y)
    }).toList.sorted
  }

  def get(coord: Coord): Cell[T] = {
    cellMap.get(coord) match {
      case Some(cell: Cell[T]) => cell
      case None => new Cell[T](coord, Option.empty) {}
    }
  }

  def add[U >: T](cell: (Coord, U)): Grid[U] = {
    createGrid(width, height, cell :: (initialCells.filterNot(initialCell => initialCell._1 == cell._1)))
  }

  private[gridmodel] def createGrid[U >: T](width: Int, height: Int, initialCells: List[(Coord, U)]): Grid[U]

  def transform[U >: T](f: (Cell[U]) => Option[U]): Grid[U] = {
    val transformedCells = for {
      cell <- cellList.par
      newCellContent = f(cell)
      if (newCellContent.isDefined)
    } yield (cell.coord, newCellContent.get)
    createGrid(width, height, transformedCells.toList)
  }

  override def toString: String = {
    cellMap map {
      case (key, value) => value
    } mkString (System.lineSeparator())
  }

  def ascii: String = {
    (for {
      y <- ((0 until height) reverse)
      x <- 0 until width
    } yield {
      val nl =
        if (x == (width - 1)) System.lineSeparator()
        else ""
      cellMap.get(x, y) match {
        case Some(cell) => if (cell.contents.isEmpty) "o " + nl else "x " + nl
        case _ => "o " + nl
      }
    }).mkString ("")
  }

  def canEqual(that: Any): Boolean

  override def equals(obj: Any) = obj match {
    case that: Grid[T] => {
      (this eq that) || canEqual(that) && this.cellMap.size == that.cellMap.size && this.width == that.width && this.height == that.height &&
        this.cellMap.keySet.forall(key => {
          that.cellMap.contains(key) && this.cellMap.get(key) == that.cellMap.get(key)
        })
    }
    case _ => false
  }

  override def hashCode(): Int = {
    if (cellMap.values.isEmpty) {
      41 * (
        41 + width.hashCode()
        ) + height.hashCode()
    }
    else {
      41 * (
        41 * (
          (cellMap.values map (value => 41 + value.hashCode())).reduceLeft((sum, value) => (41 * sum) + value)
          ) + width.hashCode()
        ) + height.hashCode()
    }
  }
}

/**
 * A standard grid.
 *
 * @param width of the grid (in cells)
 * @param height of the grid (in cells)
 * @param initialCells the initial grid contents
 */
sealed case class StandardGrid[T](width: Int, height: Int, initialCells: List[(Coord, T)]) extends AbstractGrid[T](width, height, initialCells) {

  private[gridmodel] def createGrid[U >: T](width: Int, height: Int, initialCells: List[(Coord, U)]): Grid[U] = {
    StandardGrid(width, height, initialCells)
  }

  def northOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    get(x, y + 1)
  }

  def northEastOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    get(x + 1, y + 1)
  }

  def eastOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    get(x + 1, y)
  }

  def southEastOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    get(x + 1, y - 1)
  }

  def southOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    get(x, y - 1)
  }

  def southWestOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    get(x - 1, y - 1)
  }

  def westOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    get(x - 1, y)
  }

  def northWestOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    get(x - 1, y + 1)
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[StandardGrid[T]]

}

/**
 * A wrapping grid implementation that wraps at its boundaries.
 *
 * @param width of the grid (in cells)
 * @param height of the grid (in cells)
 * @param initialCells the initial grid contents
 */
sealed case class WrappingGrid[T](width: Int, height: Int, initialCells: List[(Coord, T)]) extends AbstractGrid[T](width, height, initialCells) {

  private[gridmodel] def createGrid[U >: T](width: Int, height: Int, initialCells: List[(Coord, U)]): Grid[U] = {
    WrappingGrid(width, height, initialCells)
  }

  override def northOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    if (y + 1 == height) get(x, 0) else get(x, y + 1)
  }

  override def northEastOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    val newX = if (x + 1 == width) 0 else x + 1
    val newY = if (y + 1 == height) 0 else y + 1
    get(newX, newY)
  }

  override def eastOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    if (y + 1 == height) get(0, y) else get(x + 1, y)
  }

  override def southEastOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    val newX = if (x + 1 == width) 0 else x + 1
    val newY = if (y - 1 < 0) height - 1 else y - 1
    get(newX, newY)
  }

  override def southOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    if (y - 1 < 0) get(x, height) else get(x, y - 1)
  }

  override def southWestOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    val newX = if (x - 1 < 0) width - 1 else x - 1
    val newY = if (y - 1 < 0) height - 1 else y - 1
    get(newX, newY)
  }

  override def westOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    if (x - 1 < 0) get(width, y) else get(x - 1, y)
  }

  override def northWestOf(coord: Coord): Cell[T] = {
    val (x, y) = coord
    val newX = if (x - 1 < 0) width - 1 else x - 1
    val newY = if (y + 1 == height) 0 else y + 1
    get(newX, newY)
  }

  override def canEqual(that: Any): Boolean = {
    that.isInstanceOf[WrappingGrid[T]]
  }

}
