object SquareEquation {

  val DefaultEps: Double = Math.pow(10, -6)

  def solve(a: Double, b: Double, c: Double, eps: Double = DefaultEps): List[Double] = {
    if (Math.abs(a) < eps) {
      throw new IllegalArgumentException("a can not be 0")
    }

    if (a.isNaN || b.isNaN || c.isNaN) {
      throw new IllegalArgumentException("coef can not be NaN")
    }

    if (Set(Double.NegativeInfinity, Double.PositiveInfinity).exists(inf => a == inf || b == inf || c == inf)) {
      throw new IllegalArgumentException("coef can not be inf")
    }

    val D: Double = b * b - 4 * a * c

    if (D < -eps) {
      List.empty
    } else if (Math.abs(D) <= eps) {
      val x = -b / (2 * a)
      x :: x :: Nil
    } else {
      val x1 = (-b - Math.sqrt(D)) / (2 * a)
      val x2 = (-b + Math.sqrt(D)) / (2 * a)
      x1 :: x2 :: Nil
    }
  }

}
