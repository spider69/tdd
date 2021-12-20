import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class SquareEquationTest extends AnyWordSpecLike with Matchers {

  "square equation" should {
    "have no roots" in {
      val roots: List[Double] = SquareEquation.solve(a = 1.0, b = 0.0, c = 1.0)
      roots should be (empty)
    }

    "have two roots" in {
      val roots: List[Double] = SquareEquation.solve(a = 1.0, b = 0.0, c = -1.0)
      roots should be (-1.0 :: 1.0 :: Nil)
    }

    "have one root" in {
      val roots: List[Double] = SquareEquation.solve(a = 1.0, b = -2.0, c = 1.000000000000001)
      roots should be (1.0 :: 1.0 :: Nil)
    }

    "throw exception" when {
      "a = 0" in {
        assertThrows[IllegalArgumentException] {
          SquareEquation.solve(a = 0.000000000000001, b = 1.0, c = 1.0)
        }
      }

      "a = +Inf" in {
        assertThrows[IllegalArgumentException] {
          SquareEquation.solve(a = Double.PositiveInfinity, b = 1.0, c = 1.0)
        }
      }

      "a = -Inf" in {
        assertThrows[IllegalArgumentException] {
          SquareEquation.solve(a = Double.NegativeInfinity, b = 1.0, c = 1.0)
        }
      }

      "a = NaN" in {
        assertThrows[IllegalArgumentException] {
          SquareEquation.solve(a = Double.NaN, b = 1.0, c = 1.0)
        }
      }

      "b = +Inf" in {
        assertThrows[IllegalArgumentException] {
          SquareEquation.solve(a = 1.0, b = Double.PositiveInfinity, c = 1.0)
        }
      }

      "b = -Inf" in {
        assertThrows[IllegalArgumentException] {
          SquareEquation.solve(a = 1.0, b = Double.NegativeInfinity, c = 1.0)
        }
      }

      "b = NaN" in {
        assertThrows[IllegalArgumentException] {
          SquareEquation.solve(a = 1.0, b = Double.NaN, c = 1.0)
        }
      }

      "c = +Inf" in {
        assertThrows[IllegalArgumentException] {
          SquareEquation.solve(a = 1.0, b = 1.0, c = Double.PositiveInfinity)
        }
      }

      "c = -Inf" in {
        assertThrows[IllegalArgumentException] {
          SquareEquation.solve(a = 1.0, b = 1.0, c = Double.NegativeInfinity)
        }
      }

      "c = NaN" in {
        assertThrows[IllegalArgumentException] {
          SquareEquation.solve(a = 1.0, b = 1.0, c = Double.NaN)
        }
      }
    }
  }
}
