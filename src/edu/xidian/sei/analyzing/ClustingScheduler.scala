package edu.xidian.sei.analyzing


import akka.actor.{Actor, ActorLogging}
import edu.xidian.sei.Index

case object ProcessComplete

/**
 * Clustering multi-thread scheduler class
 *
 * @param data grid data
 * @param analyzerFactory clustering analyzer
 * @param params clustering parameters
 */
class ClustingScheduler[K <: Index, V](data: collection.Map[K, V], analyzerFactory: SliceAnalyzerFactory, params: collection.Map[String, Any])
  extends Actor with ActorLogging {

  val analyzers = analyzerFactory.build()
  var idles = analyzers.toList
  var datas = analyzerFactory.slicer.slice(data, params).toList
  val sliceCounts = datas.size
  var pending = datas.size

  override def preStart: Unit = {
    (0 until Math.min(sliceCounts, analyzers.size)) foreach (i => sendAData())
  }

  private def sendAData(): Unit = {
    if (!datas.isEmpty && !idles.isEmpty) {
      idles.head ! datas.head
      idles = idles.tail
      datas = datas.tail
    }
  }

  def receive = {
    case ProcessComplete =>
      pending -= 1
      if (pending == 0) {
        analyzers foreach (p => p ! "stop")
        context.stop(self)
      } else {
        idles = sender :: idles
        sendAData()
      }
  }
}
