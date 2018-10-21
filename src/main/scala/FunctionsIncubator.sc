import scala.annotation.tailrec
import scala.math.abs

def getFixedPointOfFunction(f: Double => Double)(firstGuess: Double): Double = {
  val tolerance: Double = 0.001

  def isCloseEnough(guess: Double, x: Double) = abs(guess * guess - x) / x < tolerance

  @tailrec
  def inner(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else inner(next)
  }

  inner(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x))/2

getFixedPointOfFunction(x => 1 + x/2)(1)

def sqrt(x: Double): Double = {

}