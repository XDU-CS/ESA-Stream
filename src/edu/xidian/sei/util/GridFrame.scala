package edu.xidian.sei.util

import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D

import javax.swing.JPanel
import scala.util.Random
import java.awt.Font
import scala.reflect.internal.util.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

/**
  * 2D clustering result display
  *
  * @param gridNum Number of grids
  * @param gridLen length of grid
  * @param frameWidth UI width
  * @param result clustering result
  * @param title UI title
  */
case class GridFrame(gridNum: Int, gridLen: Int, frameWidth: Int, result: ArrayBuffer[(Array[Int], String)], title: String) extends JPanel {

  val clusterMap = new HashMap[String, Color]()
  def this(gridNum: Int, gridLen: Int, frameWidth: Int, result: ArrayBuffer[(Array[Int], String)]) {
    this(gridNum, gridLen, frameWidth, result, "Result display")
  }

  setSize(frameWidth + gridNum * gridLen, frameWidth + gridNum * gridLen)

  override def paint(g: Graphics) {
    super.paintComponent(g);
    val g2d = g.asInstanceOf[Graphics2D]
    g2d.setFont(GridFrame.fnStyle);
    g2d.setColor(Color.BLACK);
    val strWidth = g.getFontMetrics().stringWidth(title);
    g2d.drawString(title, frameWidth + gridNum * gridLen / 2 - strWidth / 2, frameWidth / 2);
    //    g2d.setColor(Color.black);
    g2d.drawRect(frameWidth, frameWidth, gridNum * gridLen, gridNum * gridLen);

    (1 until gridNum).foreach { i =>
      g2d.drawLine(frameWidth, frameWidth + i * gridLen, frameWidth + gridNum * gridLen,
        frameWidth + i * gridLen);
      g2d.drawLine(frameWidth + i * gridLen, frameWidth, frameWidth + i * gridLen, frameWidth
        + gridNum * gridLen);
    }

    result.foreach { x =>
      if (clusterMap.contains(x._2)) {
        drawGrid(x._1(0), x._1(1), g2d, clusterMap.get(x._2).get)
      } else {
        clusterMap.put(x._2, getRandColorCode)
        drawGrid(x._1(0), x._1(1), g2d, clusterMap.get(x._2).get)
      }
    }

    g2d.dispose();
  }

  def getRandColorCode: Color = {
    val random = new Random();
    val r = random.nextInt(255);
    val g = random.nextInt(255);
    val b = random.nextInt(255);
    return new Color(r, g, b);
  }

  def drawGrid(x: Int, y: Int, g2d: Graphics2D, colorCode: Color) {
    g2d.setColor(colorCode);
    g2d.fillRect(frameWidth + gridLen * x, frameWidth + (gridNum - 1) * gridLen
      - gridLen * y, gridLen, gridLen);
    g2d.setColor(Color.black);
    g2d.drawRect(frameWidth + gridLen * x, frameWidth + (gridNum - 1) * gridLen
      - gridLen * y, gridLen, gridLen);
  }
}
object GridFrame {
  val fnStyle = new Font("Time New Roman", Font.BOLD, 20)
}