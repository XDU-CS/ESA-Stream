package edu.xidian.sei

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.control.Breaks

import breeze.linalg._
import breeze.numerics._
import scala.collection.mutable.HashSet

/**
  * Main class of PCA dimensionality reduction method
  *
  */
object PCA {

  def calcIndex(location: Location): Index = {
    //
    val gridLength = Space.getInstance.gridLength.map(x => x)
    location match {
      case Coordinate(x, y) => CoIndex(floor(x / gridLength(0)).toInt, floor(y / gridLength(1)).toInt)
      case _ =>
        val dimensions = location.dimensions
        val rs = Array.ofDim[Int](dimensions.size)
        (0 until dimensions.size) foreach { i =>
          rs(i) = floor(dimensions(i) / gridLength(i)).toInt
        }
        IndexImpl(rs)
    }
  }

  def compute(data: DenseMatrix[Double], densityArray: DenseVector[Double], accuracy: Double, dimension: Int, tc: Long): List[Index] = {

    val importData = data.copy

    val mean = Array.ofDim[Double](dimension)
    for (i <- 0 until importData.rows) {
      val col = breeze.linalg.sum(importData(i, ::).t :* densityArray) / breeze.linalg.sum(densityArray)
      mean(i) = col
    }

    for (i <- 0 until importData.rows) {
      for (j <- 0 until importData.cols) {
        importData(i, j) -= mean(i)
      }
    }

    //Compute covariance matrix
    val covarianceMatrix = DenseMatrix.zeros[Double](importData.rows, importData.rows)
    for (i <- 0 until mean.size) {
      for (j <- 0 to i) {
        covarianceMatrix(i, j) = breeze.linalg.sum(importData(i, ::) :* importData(j, ::)) / ((importData.cols - 1) * 1.0)
      }
    }
    for (i <- 0 until mean.size) {
      for (j <- i until mean.size) {
        covarianceMatrix(i, j) = covarianceMatrix(j, i)
      }
    }

    val eigenvectorArray = eig(covarianceMatrix).eigenvectors.t
    val eigenvaluesArray = eig(covarianceMatrix).eigenvalues.data
    val eigenvaluesSum = eigenvaluesArray.sum
    val eigenvaluesMap = (0 until eigenvaluesArray.length).map(i => (eigenvaluesArray(i), i)).sortWith(_._1 > _._1)

    var sum = 0.0
    val indexSet = new HashSet[Int]()
    val loop = new Breaks;
    loop.breakable {
      for (pair <- eigenvaluesMap) {
        sum += pair._1
        indexSet.add(pair._2)
        if (sum > eigenvaluesSum * accuracy)
          loop.break
      }
    }

    val finalMatrix = DenseMatrix.zeros[Double](eigenvectorArray.cols, indexSet.size)
    var num = 0
    for (i <- indexSet) {
      //      println(i)
      for (j <- 0 until eigenvectorArray.cols) {
        finalMatrix(j, num) = eigenvectorArray(i, j)
      }
      num += 1
    }

    val resultMatrix = finalMatrix.t * data

    val maxVector = Array.ofDim[Double](resultMatrix.rows)
    val minVector = Array.ofDim[Double](resultMatrix.rows)

    for (i <- 0 until resultMatrix.rows) {
      maxVector(i) = breeze.linalg.max(resultMatrix(i, ::))
      minVector(i) = breeze.linalg.min(resultMatrix(i, ::))
    }

    for (i <- 0 until resultMatrix.rows) {
      for (j <- 0 until resultMatrix.cols) {
        resultMatrix(i, j) = (resultMatrix(i, j) - minVector(i)) / (maxVector(i) - minVector(i))
      }
    }

    val gridNewIndexList = (0 until resultMatrix.cols).map(i => calcIndex(Location.of(resultMatrix(::, i).toArray))).toList
    gridNewIndexList

  }

  def pca(gridList: List[Grid], tc: Long): List[Index] = {
    val dimension = Space.getInstance.dimension
    val buffer = gridList.map(_.center.dimensions).flatMap(_.toList).toArray
    val data = new DenseMatrix(dimension, gridList.size, buffer)
    val densityArray = DenseVector(gridList.map(_.density).toArray)
    val gridNewIndexList = compute(data, densityArray, 0.93, dimension, tc)
    gridNewIndexList
  }

  /**
    * PCA test main method
    * @param args
    */
  def main(args: Array[String]): Unit = {
    val lineNum = Source.fromFile("test.txt").getLines().length
    println(lineNum)
    val file = Source.fromFile("test.txt")
    val buffer = new ArrayBuffer[Array[Double]]

    for (line <- file.getLines) {
      val location = line.split("     ").map(i ⇒ i.toDouble)
      buffer += location
    }
    val data = new DenseMatrix(4, lineNum, buffer.toList.flatMap(_.toList).toArray)
    file.close
    val m = DenseMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    println(m(::, 1))

  }
}
