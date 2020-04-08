package edu.xidian.sei

import java.io.{ File, PrintWriter }
import scala.math.BigDecimal
import javax.swing.JFrame
import edu.xidian.sei.util.GridFrame
import scala.collection.mutable.ArrayBuffer
import java.io.FileWriter
import java.text.DecimalFormat

/** Cluster  class*/
trait Cluster

case class DefaultCluster(val id: Any) extends Cluster {
  var density: BigDecimal = BigDecimal(0)
  val objects = new collection.mutable.HashSet[Any]
  var center: Array[Double] = _
  override def hashCode: Int = id.hashCode

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[DefaultCluster]
    other.id.equals(this.id)
  }

  def compute {
    center = new Array[Double](Space.getInstance.dimension)
    for (gridObj <- objects) {
      val grid = gridObj.asInstanceOf[Grid]
      density += grid.density
      center = Location.twoArrayAdd(center, grid.center.dimensions)
    }
    for (i <- 0 until Space.getInstance.dimension) {
      center(i) = center(i) / objects.size
    }

  }

  def merge(other: DefaultCluster) {
    for (i <- 0 until Space.getInstance.dimension) {
      this.center(i) = (this.center(i) * this.objects.size + other.center(i) * other.objects.size) / (this.objects.size + other.objects.size)
    }
    this.density += other.density
  }
}

/** Grid matching pattern */
trait Pattern[T] extends Function2[T, T, Boolean]

/** Match the coordinates of the two grids */
case class Matched(val first: Any, val second: Any)

/** a processor that processes a batch of grids */
trait Processor[K, V] {
  def process(keys: Iterable[K], datas: collection.Map[K, V], params: collection.Map[String, Any]): Seq[Matched]
}

/** Cluster reactor that processes all data */
trait ClustingReactor {
  def clusting(id: String, datas: collection.Map[_, _], params: collection.Map[String, Any]): Unit

  def cleanup(): Unit
}

/** Fragment data that needs to be processed at one time */
case class DataToProcess[K, V](keys: Iterable[K], datas: collection.Map[K, V], params: collection.Map[String, Any])

case class GridToProcess[K, V](datas: collection.Map[K, V], params: collection.Map[String, Any])

/** a slicer that splits the data */
trait Slicer {
  def slice[K <: Index, V](data: collection.Map[K, V], params: collection.Map[String, Any]): Seq[DataToProcess[K, V]]
}

/** Reporter */
trait Reporter {
  def report(result: collection.Map[Any, Cluster])
}

/** File reporter */
class FileReporter(val fileName: String, id: String) extends Reporter {

  def report(result: collection.Map[Any, Cluster]): Unit = {
    val filewriter = new PrintWriter(new File(fileName))
    println(s"Writing ${result.size} grid to $fileName")
    val drawResult = new ArrayBuffer[(Array[Int], String)]()
    val densityList = new ArrayBuffer[Double]
    for ((k, v) <- result) {
      val density = k.asInstanceOf[Grid].density
      densityList += density
      val gridID = k.asInstanceOf[Grid].id.dimensions
      val clusterID = v.asInstanceOf[DefaultCluster].id.asInstanceOf[Index].dimensions.mkString(",")  
      filewriter.println(gridID.mkString(",") + "=>" + clusterID)
      drawResult += (gridID -> clusterID)
    }
    filewriter.close
    
//    val out = new FileWriter("knowledge_base.csv", true)
//    val format = new DecimalFormat("0.00");
//    val densityMean = (densityList.sum / densityList.size)
//    val bigDensitySize = densityList.filter(_ >= densityMean).size
//    out.write(id + "," + format.format(densityList.max) + "," + format.format(densityMean) + "," + bigDensitySize + "," + format.format(densityList.sum) + "\n")
//    out.close()
    
    draw(drawResult, fileName) //If the data stream is high-dimensional, please comment this line。
  }
  
  def draw(result: ArrayBuffer[(Array[Int], String)], title: String) {
    val jf = new JFrame();
    jf.setSize(750, 750);
    jf.setVisible(true);
    jf.setDefaultCloseOperation(2);
    jf.getContentPane().add(new GridFrame((LocalClient.gridUpbound/LocalClient.gridLength).toInt, 10, 70, result, title))
  }
}