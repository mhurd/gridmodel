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

sealed case class Grid[T](width: Int, height: Int) {

  implicit object CellOrdering extends Ordering[Cell[T]] {
    def compare(a: Cell[T], b: Cell[T]) = {
      val xComp = a.x compare b.x
      if (xComp == 0) a.y compare b.y
      else xComp
    }
  }

  private var cellGrid: Map[(Int, Int), Cell[T]] = Map()

  def cellMap(): Map[(Int, Int), Cell[T]] = cellGrid

  override def equals(obj: Any) = obj match {
    case that: Grid[T] => {
      (this eq that) || canEqual(that) && this.cellGrid.size == that.cellGrid.size && this.width == that.width && this.height == that.height &&
        this.cellGrid.keySet.forall(key => {
          that.cellGrid.contains(key) && this.cellGrid.get(key) == that.cellGrid.get(key)
        })
    }
    case _ => false
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Grid[T]]

  override def hashCode(): Int = {
    if (cellGrid.values.isEmpty) {
      41 * (
        41 + width.hashCode()
        ) + height.hashCode()
    }
    else {
      41 * (
        41 * (
          (cellGrid.values map (value => 41 + value.hashCode())).reduceLeft((sum, value) => (41 * sum) + value)
          ) + width.hashCode()
        ) + height.hashCode()
    }
  }

  def get(x: Int, y: Int): Cell[T] = {
    cellGrid.get((x, y)) match {
      case Some(cell: GridCell) => cell
      case None => GridCell(x, y, None)
    }
  }

  def cellList: List[Cell[T]] = {
    (for {
      y <- ((0 until height) reverse)
      x <- 0 until width
    } yield {
      get(x, y)
    }).toList.sorted
  }

  def put(x: Int, y: Int, content: T): Cell[T] = {
    val newCell = GridCell(x, y, Option(content))
    if (!newCell.isOutOfBounds) cellGrid += ((newCell.x, newCell.y) -> newCell)
    newCell
  }

  def remove(x: Int, y: Int) = {
    cellGrid = cellGrid - ((x, y))
  }

  override def toString: String = {
    cellGrid map {
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
      if (cellGrid.get(x, y).isEmpty) "o " + nl
      else "* " + nl
    }) mkString ("")
  }

  private sealed case class GridCell(x: Int, y: Int, content: Option[T]) extends Cell[T] {

    def isEmpty: Boolean = content.isEmpty

    def isOutOfBounds: Boolean = (x + 1) > width || (y + 1) > height || x < 0 || y < 0

    def surroundingCells: List[Cell[T]] = {
      val b = ListBuffer[Cell[T]]()
      north +=: northEast +=: east +=: southEast +=: south +=: southWest +=: west +=: northWest +=: b
      (b.toList filter (!_.isOutOfBounds)).sorted
    }

    def north: Cell[T] = get(x, y + 1)

    def northEast: Cell[T] = get(x + 1, y + 1)

    def east: Cell[T] = get(x + 1, y)

    def southEast: Cell[T] = get(x + 1, y - 1)

    def south: Cell[T] = get(x, y - 1)

    def southWest: Cell[T] = get(x - 1, y - 1)

    def west: Cell[T] = get(x - 1, y)

    def northWest: Cell[T] = get(x - 1, y + 1)

    def contentsString[T]: String = {
      if (isOutOfBounds) "Out-of-bounds"
      else
        content match {
          case Some(c: T) => c.toString
          case None => "Empty"
        }
    }

    override def toString: String = {
      formatXY(x, y) + " = " + contentsString[T]
    }

    override def equals(obj: Any) = obj match {
      case that: Grid[T]#GridCell => {
        (this eq that) || (canEqual(that) && this.x == that.x && this.y == that.y && this.content == that.content)
      }
      case _ => false
    }

    override def canEqual(that: Any): Boolean = that.isInstanceOf[GridCell]

    override def hashCode(): Int = {
      41 * (
        41 * (
          41 + x.hashCode()
          ) + y.hashCode()
        ) + content.hashCode()
    }

  }

}


