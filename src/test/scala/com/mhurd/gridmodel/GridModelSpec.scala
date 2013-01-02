package com.mhurd.gridmodel

import org.scalatest.FlatSpec

class GridModelSpec extends FlatSpec {

  "gridmodel package object" must "be able to format x,y coordinates" in {
    expect("(1,2)") {
      formatXY(1, 2)
    }
  }

}