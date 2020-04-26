package lab5.io

import java.lang.Math._

import lab4.method.LagrangePolynomial
import lab5.chart.Chart
import lab5.method.RungeKutta4

import scala.io.StdIn

class CommandHandler {
  def handleUserInput(): Unit = {

    var nSample: Option[Int] = None
    while (nSample.isEmpty || nSample.get < 1 || nSample.get > 3) {
      println("Выберите дифф. уравнение\n" +
        "1) y'= x^2-2y\n" +
        "   Ответ при [x=0,y=1]: y = 3/4*e^(-2x)+1/2*x^2-1/2*x+1/4\n" +
        "2) y'=y*(2*sin(x)+1)\n" +
        "   Ответ при [x=0,y=1]: y = e^(2+x-2cos(x))\n" +
        "3) y'=(sin((Pi * x^2)/2))*y\n" +
        "   Не существует ответа в виде элементарной функции")
      try {
        nSample = Option(StdIn.readInt())
      } catch {
        case x: Exception => println("Введите 1, 2, или 3")
      }
    }

    var x: Option[Double] = None
    while (x isEmpty) {
      println("Ввод начального условия. Введите x0")
      x = readDoubleSafe()
    }

    var y: Option[Double] = None
    while (y isEmpty) {
      println("Ввод начального условия. Введите y0")
      y = readDoubleSafe()
    }

    var a: Option[Double] = None
    while (a.isEmpty) {
      println("Введите начало отрезка")
      a = readDoubleSafe()
    }

    var b: Option[Double] = None
    while (b.isEmpty || a.get >= b.get) {
      println("Введите конец отрезка. Он должен быть больше начала отрезка")
      b = readDoubleSafe()
    }

    var E: Option[Double] = None
    while (E.isEmpty || E.get <= 0.0) {
      println("Введите погрешность. Она должна быть положительной")
      E = readDoubleSafe()
    }
    // y'= x^2-2*y ans = 3/4*e^(-2x)+1/2*x^2-1/2*x+1/4; y'=y*(2*sin(x)+1)    ans = e^(2+x-2cos(x)); y'=(sin((Pi * x^2)/2)) * y   ans = doesn't exist
    val funList = List((x: Double, y: Double) => pow(x, 2) - 2 * y, (x: Double, y: Double) => y * (2*sin(x) + 1), (x: Double, y: Double) => (sin((PI * pow(x,2))/2)) * y)

    try {
      val XY = new RungeKutta4(fy = funList(nSample.get - 1), x = x.get, y = y.get, a = a.get, b = b.get, E = E.get).solve()
      val interF = new LagrangePolynomial(XY._1, XY._2).getFunction()
      new Chart("Метод Рунге-Кутта 4-го порядка", interF, XY._1, XY._2).drawChart()
    } catch {
      case e: Exception => println(e.getMessage)
    }
  }

  def readDoubleSafe(): Option[Double] = {
    try {
      Option(StdIn.readDouble())
    } catch {
      case x: Exception => println("Должно быть вещественным числом!")
        None
    }
  }
}
