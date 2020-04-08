package edu.xidian.sei

import java.util.ArrayList

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.forkjoin.{ ForkJoinPool, ForkJoinTask, RecursiveTask }
import scala.util.control.Breaks

/**
 * The location of the point
 */
trait Location extends Serializable {
  def dimensions: Array[Double]

  def dimensionSize: Int
}

case class LocationImpl(val dimensions: Array[Double]) extends Location {
  def dimensionSize = dimensions.length

}

object Location {
  def of(x: Array[String]): Location = {
    if (x.length == 2) Coordinate(x(0).toDouble, x(1).toDouble)
    else LocationImpl(x.map(i => i.toDouble))
  }

  def of(x: Array[Double]): Location = {
    if (x.length == 2) Coordinate(x(0), x(1))
    else LocationImpl(x)
  }

  def twoArrayAdd(array1: Array[Double], array2: Array[Double]): Array[Double] = {
    var temp = Array.ofDim[Double](array1.size)
    for (i <- 0 until array1.size) {
      temp(i) = array1(i) + array2(i)
    }
    temp
  }

  def twoArrayDistance(array1: Array[Double], array2: Array[Double]): Double = {
    var temp: Double = 0
    for (i <- 0 until array1.size) {
      temp += Math.pow(array1(i) - array2(i), 2)
    }
    temp
  }
}

/**
 * Two-dimensional coordinates
 */
case class Coordinate(val x: Double, val y: Double) extends Location {
  def dimensions: Array[Double] = {
    Array(x, y)
  }

  def dimensionSize = 2

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[CoIndex]
    this.x == other.x && this.y == other.y
  }
}

/**
 * Grid index
 */
trait Index extends Serializable {
  def dimensions: Array[Int]
  
   def isNeighbor(other: Index): Boolean = {
    var b = true
    val loop = new Breaks
    loop.breakable {
      for (i <- 0 to dimensions.size - 1) {
        if (Math.abs(this.dimensions(i) - other.dimensions(i)) > 1) {
          b = false
          loop.break
        }
      }
    }
    b
  }
}

case class IndexImpl(val dimensions: Array[Int]) extends Index {
  val hashcode = calcHashCode(dimensions)

  override def hashCode: Int = {
    hashcode
  }

  private def calcHashCode(d: Array[Int]): Int = {
    val dimensionSize = d.length
    var i = 0
    var sum = 0
    while (i < dimensionSize) {
      sum += d(i)
      i += 1
    }
    sum
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[IndexImpl]
    val dimensionSize = dimensions.length
    val me = dimensions
    val o = other.dimensions
    var i = 0
    while (i < dimensionSize) {
      if (me(i) != o(i)) return false
      i += 1
    }
    return true
  }
}

/**
 * 2D grid index
 */
case class CoIndex(val x: Int, val y: Int) extends Index {
  val hashcode = x + y

  def dimensions: Array[Int] = {
    Array(x, y)
  }

  override def hashCode: Int = {
    hashcode
  }

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[CoIndex]
    this.x == other.x && this.y == other.y
  }
}

case class Space(val dimension: Int, gridLength: Array[Double], upperBound: Array[Double]){
  val s = math.ceil(upperBound(0) / gridLength(0)).toInt
  val neighborsTotal = math.pow(3, dimension) - 1
}

object Space {
  private var neighborsMap: Map[String, collection.mutable.HashSet[Array[Int]]] = Map.empty
  private val forkJoinPool = new ForkJoinPool();
  var isComplete: Boolean = true
  var gridDistanceThreshold = 0.0
  var gridDistance = 0.0
  private var instance: Space = null
  //  var dimenSelected: Array[Int] = _

  def getInstance = instance

  def getGridAcreage = instance.gridLength.fold(1.0)((x, y) => x * y)

  def apply(dimension: Int, gridLength: Double, upperBound: Double): Space = {
    val GRID_LEN = new Array[Double](dimension) //格子长度
    val UPPERBOUND = new Array[Double](dimension) //上界
    val p = new Array[Int](dimension) //每一维格子数
    gridDistanceThreshold = gridLength * 1.5
    gridDistance = gridLength
    for (i <- 0 until dimension) UPPERBOUND(i) = upperBound
    for (i <- 0 until dimension) GRID_LEN(i) = gridLength
    for (i <- 0 until dimension) p(i) = math.ceil(UPPERBOUND(i) / GRID_LEN(i)).toInt
    for (i <- 0 until dimension) UPPERBOUND(i) = GRID_LEN(i) * p(i)
    instance = new Space(dimension, GRID_LEN, UPPERBOUND)
    instance
  }

  def neighbors(dimension: Int): collection.mutable.HashSet[Array[Int]] = {
    if (neighborsMap.contains(dimension.toString)) return neighborsMap(dimension.toString)
    val rs = new collection.mutable.HashSet[Array[Int]]
    if (isComplete) {
      rs ++= forkJoinPool.invoke(new NeighborsFinder(dimension, new collection.mutable.ArrayBuffer[Int]))
    } else {
      rs ++= calculateByDimension(dimension)
    }
    neighborsMap += (dimension.toString -> rs)
    rs
  }

  def partialNeighbors(dimension: Int, dimenSelected: Array[Int]): collection.mutable.HashSet[Array[Int]] = {
    val id = instance.dimension + "-" + dimenSelected.mkString("-")
    if (neighborsMap.contains(id)) return neighborsMap(id)
    val rs = new collection.mutable.HashSet[Array[Int]]
    val temp = forkJoinPool.invoke(new NeighborsFinder(dimension, new collection.mutable.ArrayBuffer[Int]))
    temp.foreach { array =>
      {
        val tempArray = new Array[Int](instance.dimension)
        (0 until array.size) foreach (i => tempArray(dimenSelected(i)) = array(i))
        rs += tempArray
      }
    }
    neighborsMap += (id -> rs)
    rs
  }

  def calculateByDimension(dimension: Int): collection.mutable.HashSet[Array[Int]] = {
    val output = new collection.mutable.HashSet[Array[Int]]
    for (i <- 0 until dimension) {
      val array = new Array[Int](dimension)
      array(i) = 1
      output.add(array)
      val array1 = new Array[Int](dimension)
      array1(i) = -1
      output += array1
    }
    output
  }
}

case class NeighborsFinder(dimension: Int, dArray: collection.mutable.ArrayBuffer[Int]) extends RecursiveTask[collection.mutable.HashSet[Array[Int]]] {
  override def compute(): collection.mutable.HashSet[Array[Int]] = {
    val rs = new collection.mutable.HashSet[Array[Int]]
    if (dimension > 0) {
      val tasks = new ArrayList[ForkJoinTask[collection.mutable.HashSet[Array[Int]]]]();
      for (i <- -1 to 1) {
        dArray += i
        if (dimension > 0) {
          tasks.add(new NeighborsFinder(dimension - 1, dArray.clone))
        } else {
          if (dArray exists (x => x != 0)) rs.add(dArray.toArray)
        }
        dArray.trimEnd(1)
      }
      val tasksIter = ForkJoinTask.invokeAll(tasks).iterator()
      while (tasksIter.hasNext()) {
        val task = tasksIter.next()
        rs ++= task.join
      }
    } else {
      if (dArray exists (x => x != 0)) rs.add(dArray.toArray)
    }
    rs
  }
}

