package edu.xidian.sei.analyzing

import akka.actor.{ActorRef, ActorSystem}
import edu.xidian.sei.ClustingReactor


abstract class AbstractClustingReactor extends ClustingReactor {
  val collectors = new collection.mutable.ArrayBuffer[ActorRef]

  def system: ActorSystem

  override def cleanup(): Unit = {
    import akka.pattern.ask
    import akka.util.Timeout

    import scala.concurrent.Await
    import scala.concurrent.duration.DurationInt
    implicit val timeout: Timeout = Timeout(200 seconds)
    while (collectors.nonEmpty) {
      Thread.sleep(2000)
      val removed = new collection.mutable.ArrayBuffer[ActorRef]
      collectors foreach { collector =>
        val future = (collector ? ProcessRequest).mapTo[CollectProcess]
        val process = Await.result(future, timeout.duration)
        if (process.finished) removed += collector
        println(collector.path + " finished " + (process.receivedDataCount * 100.0 / process.expectedDataCount) + "%")
      }
      collectors --= removed
    }
    system.terminate()
  }
}