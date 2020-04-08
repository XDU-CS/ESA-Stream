package edu.xidian.sei.local

import akka.actor.{ActorRef, ActorSystem, Props}
import edu.xidian.sei.{DataToProcess, Processor, Slicer}
import edu.xidian.sei.analyzing.{SliceAnalyzer, SliceAnalyzerFactory}

//Grid data slicer for multi-thread
class LocalSlicer(count: Int) extends Slicer {
  def slice[K, V](data: collection.Map[K, V], params: collection.Map[String, Any]): Seq[DataToProcess[K, V]] = {
    val countPerSlice = Math.ceil(data.size * 1.0 / count).toInt
    val keysList = data.keys.sliding(countPerSlice, countPerSlice)
    val rs = new collection.mutable.ArrayBuffer[DataToProcess[K, V]]
    for (keys <- keysList) {
      rs += DataToProcess(keys, data, params)
    }

    val empties = count - rs.size
    val empty = DataToProcess(List.empty[K], Map.empty[K, V], params)
    (0 until empties) foreach (i => rs += empty)
    rs
  }
}

class LocalSliceAnalyzerFactory(system: ActorSystem, processor: Processor[_, _], sliceCount: Int, actorCount: Int, resultCollector: ActorRef)
  extends SliceAnalyzerFactory {
  val slicer = new LocalSlicer(sliceCount)

  def build(): Seq[ActorRef] = {
    for (i <- 0 until actorCount)
      yield system.actorOf(Props(classOf[SliceAnalyzer], processor, resultCollector))
  }
}
