package lab4.method

class LagrangePolynomial(val X: List[Double], val Y: List[Double]) {
  def getFunction(): Double => Double = {
    x => {
      var sum = 0.0
      for ((y,i) <- Y.view.zipWithIndex) {
        var l = 1.0
        for ((xj, j) <- X.view.zipWithIndex)
          if (i != j)
            l *= (x - xj)/(X(i) - xj)
        sum += y*l
      }
      sum
    }
  }
}

