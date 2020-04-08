package edu.xidian.sei.stream

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.locks.ReentrantLock

import scala.math._
import edu.xidian.sei._
import edu.xidian.sei.Grid._

import scala.collection.mutable.ArrayBuffer
import java.text.DecimalFormat
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

/**
  * GridMap
  * 
  * It first maps or projects the normalized
  * data point to a grid, then calculates and updates
	* the density and its feature vector (discussed shortly)
	* of the grid online at a time interval gap between two
	* consecutive clustering updates. It then
	* produces a grid density centroid data streams, which
	* is used to reduce the dimensionality efficiently(if
	* needed). Since the centroid data streams is much
	* smaller than the original data, the dimensionality
	* reduction (using PCA) can be done very efficiently.
  * 
  *
  * @param space Grid space
  * @param reactor Clustering analysis reactor
  * @param toVisit Whether to traverse all neighbor grids
  */
abstract class GridMap(space: Space, reactor: ClustingReactor, toVisit: Boolean) {
  protected val datas = new HashMap[Index, Grid]
  val count = new AtomicLong(0)
  val tmpcount = new AtomicLong(0)
  var tgap = 0L
  val p = new Array[Int](space.dimension)
  val location2Index = new ArrayBuffer[(Location, Index)]()
  var N: BigDecimal = 1
  var gridNum: BigDecimal = 1
  var Du, Dl, Duiddle: BigDecimal = 0.0

  val readyToDelSet = new HashSet[Index]

  val minitor = new ReentrantLock

  //Initialization process
  private def init(): Unit = {
    val dimension = space.dimension
    for (i <- 0 until dimension) p(i) = ceil(space.upperBound(i) / space.gridLength(i)).toInt
    for (i <- 0 until dimension)
      N *= p(i)
      
    gridNum = N
    Dl = (2.0 / 3.0 * 1.0 / (N * (1 - LAMBDA)))
    Du = (1.0 / (N * (1 - LAMBDA)))

    val tmp = (N * (1 - LAMBDA) * Du - 1) / (N * (1 - LAMBDA) * Dl - 1)
    tgap = (if (tmp <= 0) floor(log((Dl / Du).toDouble) / log(LAMBDA)) else min(floor(log((Dl / Du).toDouble) / log(LAMBDA)), floor(log(tmp.toDouble) / log(LAMBDA)))).toLong
    Duiddle = (N * (1 - LAMBDA))

  }

  init()

  //Clustering analysis
  def snapshot(tc: Long, tcbefore: Long): Tuple3[BigDecimal, BigDecimal, collection.Map[Index, Grid]]

  //Data point mapping process
  def calcIndex(location: Location): Index = {
    val gridLength = space.gridLength
    location match {
      case Coordinate(x, y) => CoIndex(floor(x / gridLength(0)).toInt, floor(y / gridLength(1)).toInt)
      case _ =>
        val dimensions = location.dimensions
        val rs = Array.ofDim[Int](dimensions.size)
        (0 until dimensions.size) foreach { i =>
          rs(i) = floor(dimensions(i) / gridLength(i)).toInt
        }
        IndexImpl(rs)
    }
  }

  //Update grid centroid process
  def relocate(center: Location, location: Location, pow_coeff: Double, olddensity: Double, newDensity: Double): Location = {
    if (centerDistance(center, location) > 0.7 * Space.gridDistance) {
      return center
    }

    if (center.dimensionSize == 2) {
      val older = center.asInstanceOf[Coordinate]
      val newer = location.asInstanceOf[Coordinate]
      val newX = (older.x * olddensity * pow_coeff + newer.x) / newDensity
      val newY = (older.y * olddensity * pow_coeff + newer.y) / newDensity
      Coordinate(newX, newY)
    } else {
      val older = center.asInstanceOf[LocationImpl]
      val newer = location.asInstanceOf[LocationImpl]
      var temp = Array.ofDim[Double](older.dimensionSize)
      for (i <- 0 until older.dimensionSize) {
        temp(i) = (older.dimensions(i) * olddensity * pow_coeff + newer.dimensions(i)) / newDensity
      }
      LocationImpl(temp)
    }

  }


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

  def touch(location: Location)

  //The last clustering at the end of the program
  def cleanup(lastClusting: Boolean = true): Unit = {
    //println(Dl + " Dl+Du " + Du)
    val writer = new java.io.PrintWriter("location_" + count.get)
    val format = new DecimalFormat("0.00000");
    location2Index.foreach { loc =>
      val formatarray = loc._1.dimensions.map { x => format.format(x) }
      writer.println(formatarray.mkString(",") + "=>" + loc._2.dimensions.mkString(","))
    }
    location2Index.clear()
    writer.close()

    val countNow = count.get
    val countTmp = tmpcount.get()
    val rs = snapshot(countNow, countTmp)
    learn_new_parameters(rs._1, rs._2)
    if (rs._3.size > 1) {

      reactor.clusting(count.get.toString, rs._3, Map(Grid.DensityName -> Dl, "toVisit" -> toVisit))
    } else {
      val writer1 = new java.io.PrintWriter("index2new_" + countNow.toInt.toString + ".txt")
      rs._3.foreach { pair =>
        val oldGrid = pair._2

        writer1.println(oldGrid.id.dimensions.mkString(",") + "=>" + oldGrid.id.dimensions.mkString(","))
      }
      writer1.close()
      val writer = new java.io.PrintWriter("output_" + count.get.toString + ".txt")
      rs._3.foreach { pair =>
        val oldGrid = pair._2
        writer.println(oldGrid.id.dimensions.mkString(",") + "=>" + oldGrid.id.dimensions.mkString(","))
      }
      writer.close()
    }

    reactor.cleanup()
  }

  //It keeps on learning and updating the parameters (i.e., Dl, Du and gap)
  // online when new data points in the stream arrive.
  def learn_new_parameters(s1: BigDecimal, s2: BigDecimal) {
    N = datas.size
    Dl = 2.0 / 3.0 * 1.0 / (N * (1 - LAMBDA))
    Du = (4.0 * 1.0 / (N * (1 - LAMBDA)) + s1) / 5.0

    val tmp = (N * (1 - LAMBDA) * Du - 1) / (N * (1 - LAMBDA) * Dl - 1)
    val gap = floor(log((Dl / Du).toDouble) / log(LAMBDA))

    tgap += gap.toLong
  }

  def isNeighbor(x: Index, y: Index): Boolean = {
    var flag = true
    for (i <- (0 until x.dimensions.length))
      flag = flag && (math.abs(x.dimensions(i) - y.dimensions(i)) <= 1)
    flag
  }

  def variance(array: Array[(BigDecimal, Long)]): Double = {
    var ave = 0.0
    array.foreach { i =>
      ave += i._1.toDouble
    }
    ave /= array.length
    var sum = 0.0
    array.foreach { i =>
      val density = i._1.toDouble
      sum += (density - ave) * (density - ave)
    }
    sum /= array.length
    sum
  }

}

object GridMap {

  def apply(space: Space, reactor: ClustingReactor): GridMap = {
    if (space.dimension <= 4) GridMapFourDimen(space, reactor, false)
    else GridMapMoreDimen(space, reactor, false)
  }
}
