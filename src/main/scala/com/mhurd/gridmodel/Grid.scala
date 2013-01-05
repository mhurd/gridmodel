package com.mhurd.gridmodel

import collection.mutable.ListBuffer

sealed trait Cell[T] {

  def x: Int

  def y: Int

  def content: Option[T]

  def isEmpty: Boolean

  def isOutOfBounds: Boolean

  def surroundingCells: List[Cell[T]]

  def north: Cell[T]

  def northEast: Cell[T]

  def east: Cell[T]

  def southEast: Cell[T]

  def south: Cell[T]

  def southWest: Cell[T]

  def west: Cell[T]

  def northWest: Cell[T]

}

sealed trait Grid[T] {

  def width: Int

  def height: Int

  def cellMap: Map[(Int, Int), Cell[T]]

  def cellList: List[Cell[T]]

  def get(x: Int, y: Int): Cell[T]

  def transform(f: (Cell[T]) => Option[T]): Grid[T]

  def ascii: String

  def add(cell: (Int, Int, T)): Grid[T]

}

sealed abstract class AbstractGrid[T](width: Int, height: Int, initialCells: List[(Int, Int, T)]) extends Grid[T] {

  implicit object CellOrdering extends Ordering[Cell[T]] {
    def compare(a: Cell[T], b: Cell[T]) = {
      val xComp = a.x compare b.x
      if (xComp == 0) a.y compare b.y
      else xComp
    }
  }

  /*
   Holds the map of non-empty cells
   */
  val cellMap: Map[(Int, Int), Cell[T]] = (for {
    (x, y, content) <- initialCells
    newCell: Cell[T] = createCell(x, y, Some(content))
    if (!newCell.isOutOfBounds)
  } yield (x, y) -> newCell).toMap

  /*
   Gets an ordered list of all the cells that make up the grid, including empty cells
   which are constructed on the fly
   */
  def cellList: List[Cell[T]] = {
    (for {
      y <- ((0 until height) reverse)
      x <- 0 until width
    } yield {
      get(x, y)
    }).toList.sorted
  }

  def get(x: Int, y: Int): Cell[T] = {
    cellMap.get((x, y)) match {
      case Some(cell: Cell[T]) => cell
      case None => createCell(x, y, None)
    }
  }

  def add(cell: (Int, Int, T)): Grid[T] = {
    createGrid(width, height, cell :: initialCells)
  }

  private[gridmodel] def createGrid(width: Int, height: Int, initialCells: List[(Int, Int, T)]): Grid[T]

  private[gridmodel] def createCell(x: Int, y: Int, content: Option[T]): Cell[T]

  def transform(f: (Cell[T]) => Option[T]): Grid[T] = {
    val transformedCells = for {
      cell <- cellList
      newCellContent = f(cell)
      if (newCellContent.isDefined)
    } yield (cell.x, cell.y, newCellContent.get)
    createGrid(width, height, transformedCells)
  }

  override def toString: String = {
    cellMap map {
      case (key, value) => value
    } mkString ("\n")
  }

  def ascii(): String = {
    (for {
      y <- ((0 until height) reverse)
      x <- 0 until width
    } yield {
      val nl =
        if (x == (width - 1)) System.lineSeparator()
        else ""
      if (cellMap.get(x, y).isEmpty) "o " + nl
      else "x " + nl
    }) mkString ("")
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

  private[gridmodel] sealed abstract class AbstractGridCell(x: Int, y: Int, content: Option[T]) extends Cell[T] {

    def isEmpty: Boolean = content.isEmpty

    def isOutOfBounds: Boolean = (x + 1) > width || (y + 1) > height || x < 0 || y < 0

    def surroundingCells: List[Cell[T]] = {
      val b = ListBuffer[Cell[T]]()
      north +=: northEast +=: east +=: southEast +=: south +=: southWest +=: west +=: northWest +=: b
      (b.toList filter (!_.isOutOfBounds)).sorted
    }

    def contentsString[T]: String = {
      if (isOutOfBounds) "Out-of-bounds"
      else
        content match {
          case Some(c) => c.toString
          case None => "Empty"
        }
    }

    override def toString: String = {
      formatXY(x, y) + " = " + contentsString[T]
    }

    def canEqual(that: Any): Boolean

    override def equals(obj: Any) = obj match {
      case that: AbstractGrid[T]#AbstractGridCell => {
        (this eq that) || (canEqual(that) && this.x == that.x && this.y == that.y && this.content == that.content)
      }
      case _ => false
    }

    override def hashCode(): Int = {
      41 * (
        41 * (
          41 + x.hashCode()
          ) + y.hashCode()
        ) + content.hashCode()
    }

  }

  private[gridmodel] sealed case class StandardGridCell(x: Int, y: Int, content: Option[T]) extends AbstractGridCell(x, y, content) {

    def north: Cell[T] = get(x, y + 1)

    def northEast: Cell[T] = get(x + 1, y + 1)

    def east: Cell[T] = get(x + 1, y)

    def southEast: Cell[T] = get(x + 1, y - 1)

    def south: Cell[T] = get(x, y - 1)

    def southWest: Cell[T] = get(x - 1, y - 1)

    def west: Cell[T] = get(x - 1, y)

    def northWest: Cell[T] = get(x - 1, y + 1)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[StandardGridCell]

  }

  private[gridmodel] sealed case class WrappingGridCell(x: Int, y: Int, content: Option[T]) extends AbstractGridCell(x, y, content) {

    override def north: Cell[T] = if (y + 1 == height) get(x, 0) else get(x, y + 1)

    override def northEast: Cell[T] = {
      val newX = if (x + 1 == width) 0 else x + 1
      val newY = if (y + 1 == height) 0 else y + 1
      get(newX, newY)
    }

    override def east: Cell[T] = if (x + 1 == width) get(0, y) else get(x + 1, y)

    override def southEast: Cell[T] = {
      val newX = if (x + 1 == width) 0 else x + 1
      val newY = if (y - 1 < 0) height - 1 else y - 1
      get(newX, newY)
    }

    override def south: Cell[T] = if (y - 1 < 0) get(x, height) else get(x, y - 1)

    override def southWest: Cell[T] = {
      val newX = if (x - 1 < 0) width - 1 else x - 1
      val newY = if (y - 1 < 0) height - 1 else y - 1
      get(newX, newY)
    }

    override def west: Cell[T] = if (x - 1 < 0) get(width, y) else get(x - 1, y)

    override def northWest: Cell[T] = {
      val newX = if (x - 1 < 0) width - 1  else x - 1
      val newY = if (y + 1 == height) 0 else y + 1
      get(newX, newY)
    }

    override def canEqual(that: Any): Boolean = that.isInstanceOf[WrappingGridCell]

  }

}

sealed case class StandardGrid[T](width: Int, height: Int, initialCells: List[(Int, Int, T)]) extends AbstractGrid[T](width, height, initialCells) {

  private[gridmodel] def createGrid(width: Int, height: Int, initialCells: List[(Int, Int, T)]): Grid[T] = {
    StandardGrid(width, height, initialCells)
  }

  private[gridmodel] def createCell(x: Int, y: Int, content: Option[T]): Cell[T] = {
    StandardGridCell(x, y, content)
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[StandardGrid[T]]

}

sealed case class WrappingGrid[T](width: Int, height: Int, initialCells: List[(Int, Int, T)]) extends AbstractGrid[T](width, height, initialCells) {

  private[gridmodel] def createGrid(width: Int, height: Int, initialCells: List[(Int, Int, T)]): Grid[T] = {
    WrappingGrid(width, height, initialCells)
  }

  private[gridmodel] def createCell(x: Int, y: Int, content: Option[T]): Cell[T] = {
    WrappingGridCell(x, y, content)
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[WrappingGrid[T]]

}


