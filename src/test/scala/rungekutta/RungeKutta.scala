package rungekutta

import java.lang.Math._

import lab5.method.RungeKutta4
import org.scalactic.TolerantNumerics
import org.scalatest.flatspec.AnyFlatSpec


class RungeKutta extends AnyFlatSpec {
  val sampleFY = (x: Double, y: Double) => pow(x, 2) - 2*y
//  val sampleX = List(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
//  val sampleY = List(1.0, 0.819, 0.6727, 0.5566, 0.4669, 0.4009, 0.3558, 0.3299, 0.3214, 0.3289, 0.3515)

  "A RungeKutta4 class" should "calculate delta Yi" in {
    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.0001)
    val method = new RungeKutta4(fy = sampleFY, x = 0, y = 1, a = 0, b = 1, E = 0.0001)
    val ans = method.deltaYi(0, 1, 0.1)
    assert(ans === -0.180949)
  }

}
