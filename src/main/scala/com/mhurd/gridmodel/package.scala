package com.mhurd

package object gridmodel {

  private[gridmodel] type Coord = (Int, Int)

  def formatXY(x: Int, y: Int) = "(" + x + "," + y + ")"

}
