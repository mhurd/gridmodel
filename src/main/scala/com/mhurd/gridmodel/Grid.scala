package com.mhurd.gridmodel

import collection.mutable
import collection.mutable.ListBuffer

sealed case class Grid[T](width: Int, height: Int) {

  private val grid: mutable.Map[(Int, Int), GridCell] = mutable.Map()

  override def equals(obj: Any) = obj match {
    case that: Grid[T] => {
      (this eq that) || canEqual(that) && this.grid.size == that.grid.size && this.width == that.width && this.height == that.height &&
        this.grid.keySet.forall(key => {
          that.grid.contains(key) && this.grid.get(key) == that.grid.get(key)
        })
    }
    case _ => false
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Grid[T]]

  override def hashCode(): Int = {
    if (grid.values.isEmpty) {
      41 * (
        41 + width.hashCode()
        ) + height.hashCode()
    }
    else {
      41 * (
        41 * (
          (grid.values map (value => 41 + value.hashCode())).reduceLeft((sum, value) => (41 * sum) + value)
          ) + width.hashCode()
        ) + height.hashCode()
    }
  }

  def get(x: Int, y: Int): GridCell = {
    grid.get((x, y)) match {
      case Some(cell: GridCell) => cell
      case None => GridCell(x, y, None)
    }
  }

  def getGrid: Map[(Int, Int), GridCell] = grid.toMap

  def put(x: Int, y: Int, content: T): GridCell = {
    val newCell = GridCell(x, y, Option(content))
    if (!newCell.isOutOfBounds) grid += ((newCell.x, newCell.y) -> newCell)
    newCell
  }

  def remove(x: Int, y: Int) = {
    grid.remove((x, y))
  }

  override def toString: String = {
    grid map {
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
      if (grid.get(x, y).isEmpty) "o " + nl
      else "* " + nl
    }) mkString("")
  }

  sealed case class GridCell(x: Int, y: Int, content: Option[T]) {

    def isEmpty: Boolean = content.isEmpty

    def isOutOfBounds: Boolean = (x + 1) > width || (y + 1) > height || x < 0 || y < 0

    def surroundingCells: List[GridCell] = {
      val b = ListBuffer[GridCell]()
      north +=: northEast +=: east +=: southEast +=: south +=: southWest +=: west +=: northWest +=: b
      b.toList filter (!_.isOutOfBounds)
    }

    def north: GridCell = get(x, y + 1)

    def northEast: GridCell = get(x + 1, y + 1)

    def east: GridCell = get(x + 1, y)

    def southEast: GridCell = get(x + 1, y - 1)

    def south: GridCell = get(x, y - 1)

    def southWest: GridCell = get(x - 1, y - 1)

    def west: GridCell = get(x - 1, y)

    def northWest: GridCell = get(x - 1, y + 1)

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


