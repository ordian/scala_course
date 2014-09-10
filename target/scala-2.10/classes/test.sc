def mean(x: Double, y: Double) =
  (x + y) / 2

def sqrt(x: Double, tolerance: Double = 1e-6) = {
  require(x >= 0)

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double): Boolean =
    math.abs(guess * guess - x) < x * tolerance

  def improve(guess: Double): Double =
    mean(guess, x / guess)

  sqrtIter(x / 2)
}

sqrt(2, tolerance = 1e-8)
sqrt(1e-20)
sqrt(1.0e20)
sqrt(1.0e50)
