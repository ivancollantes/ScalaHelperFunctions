import scala.annotation.tailrec
import scala.math.abs

def getFixedPointOfFunction(f: Double => Double)(firstGuess: Double): Double = {
  val tolerance: Double = 0.0001

  def isCloseEnough(x: Double, y: Double) = abs((x-y)/x)/x < tolerance

  @tailrec
  def inner(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else inner(next)
  }

  inner(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x))/2

def sqrt(x: Double): Double = getFixedPointOfFunction(averageDamp(y => x/y))(1)

sqrt(2)