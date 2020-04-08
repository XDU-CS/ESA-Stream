package edu.xidian.sei

import scala.collection.mutable.ArrayBuffer
import scala.math._

import edu.xidian.sei.util.GridFrame

import javax.swing.JFrame
import javax.swing.JFrame._
/**
 * Grid class
 *
 * @param  id       Grid index
 * @param center    Grid Centroid position
 * @param density   Grid density
 * @param updatedAt Last update time
 */
case class Grid(val id: Index, var center: Location, var density: Double, var updatedAt: Long) {
  val visitedNeighbors = new collection.mutable.HashSet[Any]
  val neighbors = new collection.mutable.HashSet[Index]

  def addNeighbor(neighbor: Index): Grid = {
    neighbors.add(neighbor)
    this
  }

  def deleteNeighbor(neighbor: Index): Grid = {
    neighbors.remove(neighbor)
    this
  }

  def getNeighbors(): collection.mutable.HashSet[Index] = neighbors.clone()

  def addVisitedNeighbors(neighbors: Any): Unit = {
    visitedNeighbors += neighbors
  }

  def setNeighbors(neighbors: collection.mutable.HashSet[Index]): Grid = {
    this.neighbors.clear()
    this.neighbors ++= neighbors
    this
  }

  def isVisited(neighbors: Any): Boolean = {
    if (visitedNeighbors.contains(neighbors)) true else false
  }

  override def toString: String = {
    "id=" + id + ";center=" + center + ";density=" + density + ";updatedAt=" + updatedAt + ";neighborhood's size:" + neighbors.size
  }

  override def hashCode: Int = id.hashCode

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Grid]
    other.id.equals(this.id)
  }
}

object Grid {
  //decay factor
  val LAMBDA: Double = 0.99997
   
  val DensityName = "CM"

  def main(args: Array[String]) {
    println("lunnch time")
  }
}

/**
 * Analyzer for the relationship between grids
 */
class GridProcessor extends Processor[Index, Grid] {
  def process(keys: Iterable[Index], datas: collection.Map[Index, Grid], params: collection.Map[String, Any]): Seq[Matched] = {
    val matched = new collection.mutable.ArrayBuffer[Matched]
    val matches = new DensityPattern(params(Grid.DensityName).asInstanceOf[BigDecimal])
    keys foreach { index =>
      val grid = datas(index)
      if (matches(grid, grid)) matched += Matched(grid, grid)
      GridProcessor.findSearchScope(index, datas, params("toVisit").asInstanceOf[Boolean]) foreach {
        case (i, other) =>
          if (matches(other, grid)) matched += Matched(other, grid)
      }
    }
    matched
  }

}

object GridProcessor {
  def findSearchScope[K <: Index, V](k: K, datas: collection.Map[K, V], toVisit: Boolean): collection.mutable.Map[K, V] = {
    val rs = new collection.mutable.HashMap[K, V]
    val dimension = k.dimensions.size
    val dimensions = k.dimensions

    val gridOfk = datas.get(k).get.asInstanceOf[Grid]
    if (toVisit) {
      val neighbors = gridOfk.neighbors
      neighbors.foreach { x =>

        val neighborIndex = x.asInstanceOf[K]
        if (!gridOfk.isVisited(neighborIndex) && datas.contains(neighborIndex)) {
          datas.get(neighborIndex).get.asInstanceOf[Grid].addVisitedNeighbors(k)
          gridOfk.addVisitedNeighbors(x)
          rs ++= datas.get(neighborIndex).map(g => (neighborIndex, g))
        }
      }
    } else {
      val neighbors = Space.neighbors(dimension)
      neighbors.foreach { x =>
        val tempIndex =
          if (2 == dimension) {
            CoIndex(x(0) + dimensions(0), x(1) + dimensions(1)).asInstanceOf[K]
          } else {
            IndexImpl(twoArrayAdd(x, dimensions)).asInstanceOf[K]
          }

        if (!gridOfk.isVisited(tempIndex) && datas.contains(tempIndex)) {
          datas.get(tempIndex).get.asInstanceOf[Grid].addVisitedNeighbors(k)
          gridOfk.addVisitedNeighbors(tempIndex)
          rs ++= datas.get(tempIndex).map(g => (tempIndex, g))
        }
      }
    }

    rs
  }

  def twoArrayAdd(array1: Array[Int], array2: Array[Int]): Array[Int] = {
    var temp = Array.ofDim[Int](array1.size)
    for (i <- 0 until array1.size) {
      temp(i) = array1(i) + array2(i)
    }
    temp
  }

  def twoArraySubtract(array1: Array[Int], array2: Array[Int]): Array[Int] = {
    var temp = Array.ofDim[Int](array1.size)
    for (i <- 0 until array1.size) {
      temp(i) = array1(i) - array2(i)
    }
    temp
  }
}

class DensityPattern(val densityThreshold: BigDecimal) extends Pattern[Grid] {
  def apply(g1: Grid, g2: Grid): Boolean = {
    
    relationOne(g1, g2) || relationTwo(g1, g2) || relationThree(g1, g2)
  }

  def relationOne(g1: Grid, g2: Grid): Boolean = g1.density > densityThreshold && g2.density > densityThreshold && centerDistance(g1.center, g2.center) < Space.gridDistance * 1.7
  

  def relationTwo(g1: Grid, g2: Grid): Boolean = (g1.density > densityThreshold || g2.density > densityThreshold) && centerDistance(g1.center, g2.center) < Space.gridDistance * 1.3


  def relationThree(g1: Grid, g2: Grid): Boolean = g1.density <= densityThreshold && g2.density <= densityThreshold && g1.density + g2.density >= densityThreshold && centerDistance(g1.center, g2.center) < Space.gridDistance


  def centerDistance(l1: Location, l2: Location): Double = {
    val dimension = l1.dimensionSize
    val dimension1 = l1.dimensions
    val dimension2 = l2.dimensions
    var d = 0.0
    (0 until dimension).foreach { i =>
      d = d + pow(dimension1(i) - dimension2(i), 2)
    }
    pow(d, 0.5)
  }

  def centerDistanceH(l1: Location, l2: Location): Double = {
    val dimension = l1.dimensionSize
    val dimension1 = l1.dimensions
    val dimension2 = l2.dimensions
    var d = 0.0
    (0 until dimension).foreach { i =>
      d = d + abs(dimension1(i) - dimension2(i))
    }
    d
  }

  def attraction2(l1: Location, l2: Location): Boolean = {
    val dimension = l1.dimensionSize
    val dimension1 = l1.dimensions
    val dimension2 = l2.dimensions

    var tem = true
    var i = 0
    while (i < dimension && tem) {
      tem &= !(abs(dimension1(i) - dimension2(i)) > Space.gridDistanceThreshold)
      i += 1
    }
    tem
  }

  def computeMobileGridDensity(g1: Grid, g2: Grid): Double = {
    g1.density * computeAcreage(g1.id, g1.center, GridProcessor.twoArraySubtract(g2.id.dimensions, g1.id.dimensions)) / Space.getGridAcreage
  }

  def computeAcreage(gridID: Index, centroid: Location, offsetCoordinate: Array[Int]): Double = {
    val gridLength: Double = 3
    val center = gridID.dimensions.map(x => x + gridLength / 2)
    var acreage: Double = 1
    for (i <- 0 until gridID.dimensions.size) {
      val length = (centroid.dimensions(i) - center(i)) * offsetCoordinate(i)
      if (length < 0) {
        acreage *= 0
      } else {
        acreage *= (1 - Math.abs(offsetCoordinate(i))) * gridLength + mapFunction(offsetCoordinate(i)) * Math.abs(centroid.dimensions(i) - center(i))
      }
    }
    acreage
  }

  def mapFunction(arg: Int): Int = {
    val x = Math.abs(arg)
    if (x != 0) x else -1
  }
}

