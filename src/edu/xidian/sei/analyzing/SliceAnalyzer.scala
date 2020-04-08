package edu.xidian.sei.analyzing

import akka.actor.{Actor, ActorRef, actorRef2Scala}
import edu.xidian.sei.{DataToProcess, Processor, Slicer}

trait SliceAnalyzerFactory {
  def slicer: Slicer

  def build(): Seq[ActorRef]
}


class SliceAnalyzer(processor: Processor[Any, Any], resultCollector: ActorRef) extends Actor {
  def receive = {
    case DataToProcess(keys, datas, params) =>
      resultCollector ! processor.process(keys, datas, params)
      sender ! ProcessComplete
    case "stop" => context.stop(self)
  }
}