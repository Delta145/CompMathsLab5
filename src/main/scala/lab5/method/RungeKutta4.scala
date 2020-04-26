package lab5.method

import java.lang.Math._

import scala.collection.mutable.ListBuffer;

class RungeKutta4(fy: (Double, Double) => Double, x: Double, y: Double, a: Double, b: Double, E: Double) {
  def solve() = {
    val h = pow(E, 1/4.0)
    val diff = b - a
    if (h > diff)
      throw new IllegalArgumentException(s"Задана слишком большая погрешность для такого промежутка. Длина требуемого шага = $h, длина промежутка = $diff")
    val count = ((b - a)/h).toInt + 1
    var yi = y
    val Y = ListBuffer[Double]()

    val X = for (i <- 0 until count) yield a + h * i

    for (x <- X) {
      Y.addOne(yi)
      yi = yi + deltaYi(x, yi, h)
    }

    (X.toList, Y.toList)
  }

  def deltaYi(x: Double, y: Double, h: Double): Double = {
    val k1 = fy(x, y)
    val k2 = fy(x + h/2, y + h/2*k1)
    val k3 = fy(x + h/2, y + h/2*k2)
    val k4 = fy(x + h, y + h*k3)
    h/6*(k1+2*k2+2*k3+k4)
  }
}
