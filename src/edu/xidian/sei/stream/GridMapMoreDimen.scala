package edu.xidian.sei.stream

import java.io.PrintWriter
import java.util.function.BiFunction

import edu.xidian.sei.Grid._
import edu.xidian.sei._

import scala.math.{ BigDecimal, _ }
import java.text.DecimalFormat
import scala.collection.mutable.ArrayBuffer
import breeze.linalg.DenseMatrix
import scala.util.control.Breaks
import breeze.linalg.eig
import breeze.linalg.DenseVector
import scala.collection.mutable.HashSet
import scala.sys.ShutdownHookThread
import edu.xidian.sei.util.Stopwatch

/**
 * Higher dimension data stream
 * @param space Grid space
 * @param reactor Clustering analysis reactor
 * @param toVisit Whether to traverse all neighbor grids
 */
case class GridMapMoreDimen(space: Space, reactor: ClustingReactor, toVisit: Boolean) extends GridMap(space, reactor, toVisit) {

  override def touch(location: Location): Unit = {
    val countNow = count.incrementAndGet()
    val countTmp = tmpcount.get()
    if (countNow == tgap) {
      val writer = new java.io.PrintWriter("location_" + tgap)
      val format = new DecimalFormat("0.00000");
      location2Index.foreach { loc =>
        val formatarray = loc._1.dimensions.map { x => format.format(x) }
        writer.println(formatarray.mkString(",") + "=>" + loc._2.dimensions.mkString(","))
      }
      location2Index.clear()
      writer.close()
      val rs = snapshot(countNow, countTmp)
      learn_new_parameters(rs._1, rs._2)
      if (rs._3.size > 1) {
        reactor.clusting(countNow.toInt.toString, rs._3, Map(Grid.DensityName -> Du, "toVisit" -> toVisit))
      } else {
        val writer = new java.io.PrintWriter("index2new_" + countNow.toInt.toString + ".txt")
        rs._3.foreach { pair =>
          val oldGrid = pair._2

          writer.println(oldGrid.id.dimensions.mkString(",") + "=>" + oldGrid.id.dimensions.mkString(","))
        }
        writer.close()
        val writer1 = new java.io.PrintWriter("output_" + countNow.toInt.toString + ".txt")
        rs._3.foreach { pair =>
          val oldGrid = pair._2

          writer1.println(oldGrid.id.dimensions.mkString(",") + "=>" + oldGrid.id.dimensions.mkString(","))
        }
        writer1.close()
      }

      tmpcount.set(countNow)
    }

    val gridIndex = calcIndex(location) //location-->gridId

    val neighborsIndexSet = new collection.mutable.HashSet[Index]

    val index = calcIndex(location)
    location2Index += Tuple2(location, index)
    if (datas.contains(index)) {
      val grid = datas(index)
      val tc = count.get
      val pow_coeff = pow(LAMBDA, tc - grid.updatedAt)
      val tem_D = pow_coeff * grid.density + 1.0
      grid.center = relocate(grid.center, location, pow_coeff, grid.density, tem_D)
      grid.density = tem_D
      grid.updatedAt = tc
      //grid.setNeighbors(grid.getNeighbors())
    } else {
      datas.put(index, Grid(index, location, 1, count.get))
    }
  }

  override def snapshot(tc: Long, tcbefore: Long): Tuple3[BigDecimal, BigDecimal, collection.Map[Index, Grid]] = {
    var ret = 0.0
    var cnt = 0
    val keys = datas.keys
    val map = new collection.mutable.HashMap[Index, Grid]
    val selectedGridList = new ArrayBuffer[Grid]
    datas.keys.foreach { key =>
      val grid = datas(key)
      val newGridDensityDensity = pow(LAMBDA, tc - grid.updatedAt) * grid.density
      if (newGridDensityDensity <= Dl * (1 - pow(LAMBDA, tc - grid.updatedAt + 1))) {
        if (readyToDelSet.contains(grid.id)) {
          datas.remove(grid.id)
        } else {
          readyToDelSet.add(grid.id)
        }
      } else {
        if (newGridDensityDensity > Dl) {

          selectedGridList += Grid(key, grid.center, pow(LAMBDA, tc - grid.updatedAt) * grid.density, tc)
          ret += newGridDensityDensity
          cnt += 1
        }
      }
    }

    if (selectedGridList.size > 1) {
      val writer = new java.io.PrintWriter("index2new_" + tc)
      val tempList = PCA.pca(selectedGridList.toList, tc)
      val start = System.nanoTime()
      (0 until selectedGridList.size).foreach { i =>
        val newIndex = tempList(i)
        val oldGrid = selectedGridList(i)
        writer.println(oldGrid.id.dimensions.mkString(",") + "=>" + newIndex.dimensions.mkString(","))
        if (map.contains(newIndex)) {
          val savedGrid = map.get(newIndex).get
          val newGrid = computeNewGridCenter(oldGrid, savedGrid, tc)
          map.put(newIndex, newGrid)
        } else {
          val newGrid = Grid(newIndex, oldGrid.center, pow(LAMBDA, tc - oldGrid.updatedAt) * oldGrid.density, tc)
          map.put(newIndex, newGrid)
        }
      }
      println(Stopwatch.format(System.nanoTime() - start))
      writer.close()
    } else {
      val writer = new java.io.PrintWriter("index2new_" + tc)
      (0 until selectedGridList.size).foreach { i =>
        val oldGrid = selectedGridList(i)
        map.put(oldGrid.id, oldGrid)
        writer.println(oldGrid.id.dimensions.mkString(",") + "=>" + oldGrid.id.dimensions.mkString(","))
      }
      writer.close()
    }
    (ret / cnt, 0, map)
  }

  def computeNewGridCenter(g1: Grid, g2: Grid, tc: Long): Grid = {
    val gridDimension = g1.center.dimensionSize
    val density1 = g1.density * pow(LAMBDA, tc - g1.updatedAt)
    val density2 = g2.density * pow(LAMBDA, tc - g2.updatedAt)
    val sumDensity = density1 + density2
    val temp = Array.ofDim[Double](gridDimension)
    for (i <- 0 until gridDimension) {
      temp(i) = (g1.center.dimensions(i) * density1 + g2.center.dimensions(i) * density2) / sumDensity
    }
    val newGrid = Grid(g2.id, LocationImpl(temp), sumDensity, tc)
    
    newGrid
  }

}
