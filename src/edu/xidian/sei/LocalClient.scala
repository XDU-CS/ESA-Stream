package edu.xidian.sei

import java.io.File

import scala.io.Source
import scala.sys.ShutdownHookThread

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import edu.xidian.sei.local.LocalClustingReactor
import edu.xidian.sei.stream.GridMap
import edu.xidian.sei.util.Stopwatch
import java.util.concurrent.TimeUnit
import java.util.Date
import java.text.SimpleDateFormat

/**
  * LocalClient
  * Main class of ESA-Stream algorithm running
  * It accepts each data point from a stream in real-time.
  * 
  */
object LocalClient {

  val sdf = new SimpleDateFormat("yyyy.MM.dd-HH:mm:ss.SSS")
  var gridLength = 0.02
  var gridUpbound =1.0

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage:LocalClient /path/to/your/file")
      return
    }
    val start = System.nanoTime()
    val dir = new File(args(0)).getParent
    val output = dir + (if (dir.endsWith(File.separator)) "output" else "/output")

    val prompts = new StringBuilder
    
    prompts.append("Data stream: " + args(0))

    //dimension parameter
    val dimension = args(1)
    prompts.append(" Dimension=" + dimension)

    //Grid length parameter
    val length = args(2);
    gridLength = length.toDouble
    prompts.append(" Length=" + length)

    //Numerical range upper bound
    val upbound = args(3)
    gridUpbound = upbound.toDouble
    prompts.append(" Upbound=" + upbound)

    if (args.size == 5) {
      Space.isComplete = args(4).toBoolean
      prompts.append(" IsComplete=" + Space.isComplete)
    }
    println("Parameters: "+prompts.toString)
    println()

    val gridMap = GridMap(Space(dimension.toInt, length.toDouble,
      upbound.toDouble), new LocalClustingReactor(output))
    val system = ActorSystem.apply("localclient")
    val gridManager = system.actorOf(Props(classOf[GridManager], gridMap, system))
    val filereader = Source.fromFile(args(0)).getLines().toList
    val dataBlock = filereader.sliding(100000, 100000).toList
    val startTime = System.nanoTime()
    var datanum = 0
    dataBlock.foreach(block => {
      datanum += 1
      gridManager ! block
      
      TimeUnit.SECONDS.sleep(1)
    })

    gridManager ! "finish"

    ShutdownHookThread {
      println(Stopwatch.format(System.nanoTime() - startTime))
    }
  }

}

/**
 * Grid Manager
 * Grid manager receives data from data stream acceptor.
 * 
 */
class GridManager(gridMap: GridMap, system: ActorSystem) extends Actor {

  def receive = {
    case block: List[_] => {
      for (line <- block) {
        val str = line.asInstanceOf[String]
        gridMap.touch(Location.of(str.split("[;|,|\\s]")))
      }
    }
    case "finish" =>
      println("finish")
      gridMap.cleanup()
      context.stop(self)
      system.terminate()
  }
}