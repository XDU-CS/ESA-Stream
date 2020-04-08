package edu.xidian.sei.stream

import java.text.DecimalFormat
import java.util.function.BiFunction

import edu.xidian.sei.Grid._
import edu.xidian.sei._

import scala.math._

/**
  * Lower dimension data stream
  * 
  * @param space Grid space
  * @param reactor Clustering analysis reactor
  * @param toVisit Whether to traverse all neighbor grids
  */
case class GridMapFourDimen(space: Space, reactor: ClustingReactor, toVisit: Boolean) extends GridMap(space, reactor, toVisit) {

  override def touch(location: Location) {
    val countNow = count.incrementAndGet()
    val countTmp = tmpcount.get()
    if (countNow == tgap) {
      val writer = new java.io.PrintWriter("location_" + tgap)
      val format = new DecimalFormat("0.0000");
      location2Index.foreach { loc =>
        val formatarray = loc._1.dimensions.map { x => format.format(x) }
        writer.println(formatarray.mkString(",") + "=>" + loc._2.dimensions.mkString(","))
      }
      location2Index.clear()
      writer.close()
      val rs = snapshot(countNow, countTmp)
      learn_new_parameters(rs._1, rs._2)
      if (rs._3.size != 0) {
        reactor.clusting(countNow.toString, rs._3, Map(Grid.DensityName -> Dl, "toVisit" -> toVisit))
      } else {
        println("No grid can be clustered!")
      }

      tmpcount.addAndGet(countNow)
    }

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
    } else {
      datas.put(index, Grid(index, location, 1, count.get))
    }

  }

  override def snapshot(tc: Long, tcbefore: Long): Tuple3[BigDecimal, BigDecimal, collection.Map[Index, Grid]] = {
    var ret = 0.0
    var cnt = 0
    val keys = datas.keys
    val map = new collection.mutable.HashMap[Index, Grid]

    datas.keys.foreach { key =>
      val grid = datas(key)
      val newGrid = Grid(key, grid.center, pow(LAMBDA, tc - grid.updatedAt) * grid.density, tc)
      if (newGrid.density <= Dl * (1 - pow(LAMBDA, tc - grid.updatedAt + 1))) {
        if (readyToDelSet.contains(grid.id)) {
          datas.remove(grid.id)
        } else {
          readyToDelSet.add(grid.id)
        }
      } else if (newGrid.density > Dl && newGrid.density < Du) {
        map.put(key, newGrid)
        ret += newGrid.density
        cnt += 1
      } else if (newGrid.density >= Du) {
        ret += newGrid.density
        cnt += 1
        map.put(key, newGrid)
      }
    }
    (ret / cnt, 0, map)
  }
}
