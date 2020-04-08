package edu.xidian.sei.analyzing

import akka.actor.{ Actor, ActorLogging }
import edu.xidian.sei.{ DefaultCluster, Matched, Reporter }
import edu.xidian.sei.util.Stopwatch
import edu.xidian.sei.Grid
import edu.xidian.sei.Location
import edu.xidian.sei.Index
import edu.xidian.sei.Index
import java.text.SimpleDateFormat
import java.util.Date
import edu.xidian.sei.FileReporter

case class ReportRequest(filePrefix: String, dataSize: Int)
case class CollectProcess(receivedDataCount: Int, expectedDataCount: Int) {
  def finished: Boolean = {
    receivedDataCount == expectedDataCount
  }
}
case object ProcessRequest

/**
  * Result collector
  * 
  *  It integrates the intermediate clustering results and outputs the final clusters for the gap.
  * 
  * @param reporter result output
  * @param expectedDataCount Total number of slice
  */
class ResultCollector(reporter: Reporter, expectedDataCount: Int) extends Actor with ActorLogging {
  val data2Cluster = new collection.mutable.HashMap[Any, DefaultCluster]
  val clusterSet = new collection.mutable.HashSet[DefaultCluster]
  var receivedDataCount = 0

  val start: Long = System.nanoTime()

  private def generateNewCluster(a: Any, b: Any): DefaultCluster = {
    val cluster = new DefaultCluster(a.asInstanceOf[Grid].id)
    clusterSet += cluster
    addCluster(a, cluster)
    addCluster(b, cluster)
    cluster
  }

  private def addCluster(obj: Any, cluster: DefaultCluster): Unit = {
    data2Cluster.put(obj, cluster)
    cluster.objects += obj
  }

  private def merge(cluster1: DefaultCluster, cluster2: DefaultCluster): Unit = {
    if (!(cluster1 eq cluster2)) {
      if (cluster1.objects.size > cluster2.objects.size) {

        clusterSet.remove(cluster2)
        //        cluster1.merge(cluster2)

        cluster1.objects ++= cluster2.objects
        cluster2.objects foreach { o => data2Cluster.put(o, cluster1) }
      } else {

        clusterSet.remove(cluster1)
        //        cluster2.merge(cluster1)

        cluster2.objects ++= cluster1.objects
        cluster1.objects foreach { o => data2Cluster.put(o, cluster2) }
      }
    }
  }

  private def miniClusterMerge(cluster1: DefaultCluster, cluster2: DefaultCluster): Unit = {

    clusterSet.remove(cluster2)
    cluster1.merge(cluster2)

    cluster1.objects ++= cluster2.objects
    cluster2.objects foreach { o => data2Cluster.put(o, cluster1) }
  }

  private def delete(cluster1: DefaultCluster) {
    clusterSet.remove(cluster1)
    cluster1.objects.foreach {
      o => data2Cluster.remove(o)
    }
  }

  def receive = {
    case rs: Seq[_] =>
      //val start = System.nanoTime()
      rs foreach { m =>
        val a = m.asInstanceOf[Matched].first
        val b = m.asInstanceOf[Matched].second
        data2Cluster.get(a) match {
          case Some(cluster) =>
            data2Cluster.get(b) match {
              case None           => addCluster(b, cluster)
              case Some(cluster2) => merge(cluster, cluster2)
            }
          case None =>
            data2Cluster.get(b) match {
              case Some(cluster) =>
                data2Cluster.get(a) match {
                  case None           => addCluster(a, cluster)
                  case Some(cluster2) => merge(cluster, cluster2)
                }
              case None => generateNewCluster(a, b)
            }
        }
      }
      receivedDataCount += 1
      //log.info(s"receive data  $receivedDataCount/$expectedDataCount using ${Stopwatch.format(System.nanoTime - start, 4)}")
      tryReport()

    case ProcessRequest =>
      sender ! CollectProcess(receivedDataCount, expectedDataCount)
      if (receivedDataCount == expectedDataCount) context.stop(self)

    case _ => throw new RuntimeException("Unknow message type")
  }

  private def tryReport(): Unit = {
    val sdf = new SimpleDateFormat("yyyy.MM.dd-HH:mm:ss.SSS")
    if (receivedDataCount == expectedDataCount) {
      mergeCluster
      println("clusters size:" + clusterSet.size)
      reporter.report(data2Cluster)
      val elapse = System.nanoTime() - start
      println()
      log.debug(s"clusting $expectedDataCount matched data using:" + Stopwatch.format(elapse, 4))
    }
  }

  private def mergeCluster() {
    if (clusterSet.size > 2) {
      for (cluster <- clusterSet) {
        cluster.compute
      }
      val clusterSortedList = clusterSet.toList.sortWith((cluster1, cluster2) => cluster1.density > cluster2.density)
//      (0 until (if (clusterSortedList.size > 10) 10 else clusterSortedList.size)).foreach(i => { println("density:" + clusterSortedList(i).density) })
//      println(clusterSortedList(0).objects.size)
      val adjacentDifference = clusterSortedList.zip(clusterSortedList.tail).map(p => (p._2, (p._1.density - p._2.density) / p._2.density))
      var max: (DefaultCluster, BigDecimal) = (clusterSortedList(0), BigDecimal(0))
      adjacentDifference.foreach(p => if (p._2 > max._2) { max = p })
      val bigClusters = clusterSortedList.filter(x => x.density >= max._1.density)
      val miniclusters = clusterSortedList.filter(x => x.density < max._1.density)
      miniclusters.foreach { minicluster =>
        val distances = bigClusters.map { bigCluster => (Location.twoArrayDistance(bigCluster.center, minicluster.center), bigCluster) }
        val firstCluster = distances.sortWith((distance1, distance2) => distance1._1 < distance2._1)(0)._2
        miniClusterMerge(firstCluster, minicluster)
                delete(minicluster)
      }
    }
  }

}