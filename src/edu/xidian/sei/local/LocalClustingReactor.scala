package edu.xidian.sei.local

import akka.actor.{ ActorSystem, Props }
import edu.xidian.sei.analyzing.{ AbstractClustingReactor, ClustingScheduler, ResultCollector }
import edu.xidian.sei.{ FileReporter, GridProcessor }
import edu.xidian.sei.DensityPattern
import edu.xidian.sei.DataToProcess
import edu.xidian.sei.DefaultCluster
import akka.actor.Actor
import scala.util.control.Breaks
import edu.xidian.sei.Grid
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer
import edu.xidian.sei.Index
import scala.collection.mutable.HashSet
import edu.xidian.sei.Space
import edu.xidian.sei.GridToProcess
import scala.collection.mutable.Queue

/**
 * Parallel Clustering Engine 
 * 
 *  After each gap , it intelligently clusters the grid 
 *  density centroid data (not the original data points) 
 *  from Grid Manager in parallel. It then generates the 
 *  intermediate clustering results.
 */
class LocalClustingReactor(outputFilePrefix: String) extends AbstractClustingReactor {
  val system = ActorSystem.apply("reactor")

  def clusting(id: String, datas: collection.Map[_, _], params: collection.Map[String, Any]): Unit = {
    val schedulerName = "scheduler-" + id
    val outputfileName = ".\\" + "output_" + id + ".txt"
    println("clusting " + datas.size + " datas using " + schedulerName)
    val sliceCount = (datas.size / 200).toInt + 1
    val actorCount = (datas.size / 200).toInt + 1
    val collector = system.actorOf(Props(classOf[ResultCollector], new FileReporter(outputfileName, id), sliceCount), "collector-" + id)
    collectors += collector
    val analyzerFactory = new LocalSliceAnalyzerFactory(system, new GridProcessor, sliceCount, actorCount, collector)
    system.actorOf(Props(classOf[ClustingScheduler[Any, Any]], datas, analyzerFactory, params), schedulerName)
  }

  def clustering2(id: String, data: collection.Map[_, _], params: collection.Map[String, Any]): Unit = {
    val schedulerName = "scheduler-" + id
    val outputFileName = outputFilePrefix + "_" + id + ".txt"
    println("clustering " + data.size + " data using " + schedulerName)
    val collector = system.actorOf(Props(classOf[ResultCollector], new FileReporter(outputFileName, id), data.size), "collector-" + id)
    collectors += collector
    val clusteringProcessor = system.actorOf(Props(classOf[ClusteringProcessor], collector), schedulerName)
    clusteringProcessor ! GridToProcess(data, params)

  }

}

class ClusteringProcessor(resultCollector: ActorRef) extends Actor {
  override def receive: Receive = {
    case GridToProcess(data, params) =>
      val matches = new DensityPattern(params(Grid.DensityName).asInstanceOf[BigDecimal])
      val gridMap = data.asInstanceOf[collection.mutable.HashMap[Index, Grid]]
      val data2Cluster = new collection.mutable.HashMap[Grid, DefaultCluster]
      val searchBand = HashSet.empty[Index] ++ gridMap.keySet
      gridMap.keys.foreach { tempIndex =>
        if (searchBand.contains(tempIndex)) {
          searchBand.remove(tempIndex)
          val queue = new Queue[Index]()
          queue.enqueue(tempIndex)
          while (queue.nonEmpty) {
            val index = queue.dequeue()
            val grid = gridMap(index)
            resultCollector ! Tuple2(grid, tempIndex)
            findNeighbors(searchBand, index).foreach { i =>
              val g = gridMap(i)
              if (matches(grid, g)) {
                searchBand.remove(i)
                queue.enqueue(i)
              }
            }
          }
        }
      }
      //resultCollector ! data2Cluster
      context.stop(self)
  }

  def findNeighbors(searchBand: HashSet[Index], index: Index): List[Index] = {
    val neighbors = new ArrayBuffer[Index]
    val loop = new Breaks
    var num = 0
    loop.breakable {
      for (i <- searchBand) {
        if (index.isNeighbor(i)) {
          neighbors += i
          num += 1
          if (num >= Space.getInstance.neighborsTotal)
            loop.break
        }
      }
    }
    neighbors.toList
  }

}