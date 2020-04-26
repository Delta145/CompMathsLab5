package lab5.chart

import java.awt.Color
import java.awt.geom.Ellipse2D

import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.XYSplineRenderer
import org.jfree.chart.ui.{ApplicationFrame, RectangleInsets}
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
import org.jfree.data.xy.{XYDataset, XYSeries, XYSeriesCollection}

class Chart(title: String, interF: Double => Double, datasetX: List[Double], datasetY: List[Double]) extends ApplicationFrame("Lab5") {
  def drawChart(): Unit = {
    val chart = createChart()
    val panel = new ChartPanel(chart)
    panel.setPreferredSize(new java.awt.Dimension(560, 480))
    setContentPane(panel)
    pack()
    setVisible(true)
  }

  def createChart(): JFreeChart = {
    val chart = ChartFactory.createXYLineChart(
      title,
      "x",
      "y",
      null,
      PlotOrientation.VERTICAL,
      true,
      false,
      false)

    val plot = chart.getXYPlot
    plot.setBackgroundPaint(new Color(159, 190, 237))

    val rInterpolated = new XYSplineRenderer()

    rInterpolated.setSeriesPaint(0, Color.BLUE)
    rInterpolated.setSeriesShapesVisible(0, false)

    val rPoints = new XYSplineRenderer()

    rPoints.setSeriesPaint(0, Color.RED)
    rPoints.setSeriesShapesVisible(0, true)
    rPoints.setSeriesLinesVisible(0, false)
    rPoints.setSeriesShape(0, new Ellipse2D.Double(-3, -3, 6, 6))

    val a = datasetX.head; val b = datasetX.last
    val dataset2 = calcFuncDataset(a, b, interF, "interF(x)")
    val points = createDatasetFromLists(datasetX, datasetY)

    plot.setDataset(1, dataset2)
    plot.setDataset(2, points)

    // Подключение Spline Renderer к набору данных
    plot.setRenderer(1, rInterpolated)
    plot.setRenderer(2, rPoints)

    chart
  }

  def createDatasetFromLists(listX: List[Double], listY: List[Double]): XYDataset = {
    val dataset = new XYSeries(new String("sample"))
    for ((x,i) <- listX.view.zipWithIndex) {
      dataset.add(x, listY(i))
    }

    val series = new XYSeriesCollection()
    series.addSeries(dataset)

    series
  }

  def calcFuncDataset(a: Double, b: Double, f: Double => Double, desc: String): XYDataset = {
    val step = Math.abs(b - a) / 100
    val dataset = new XYSeries(new String(desc))
    var i = a
    while (i <= b) {
      dataset.add(i, f(i))
      i += step
    }

    val series = new XYSeriesCollection()
    series.addSeries(dataset)

    series
  }
}
