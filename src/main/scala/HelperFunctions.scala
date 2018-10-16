import scala.math._

object HelperFunctions {

  /*
   * foldLeft takes 2 arguments:
       ** an initial value (in this case an empty list declared as "List[Int]()") for the result and
       ** a pre-defined combining operation that, again, takes 2 arguments:
           *** the result of the operation and
           *** the current value of the list
   * EXPLANATION:
       ** "element :: result" returns result with element appended at the beginning
   */
  def reverseList(arr: List[Int]): List[Int] = arr.foldLeft(List[Int]()) { (result, element) => element :: result }

  def sumOddValuesOfList(arr: List[Int]): Int = arr.filter(_ % 2 != 0).sum

  def countNumberOfElementsInList(arr: List[Int]): Int = arr.foldLeft(0) { (count, _) => count + 1}

  def getAbsoluteValues(arr: List[Int]): List[Int] = arr.map(_.abs)

  def computeFactorial(number: Int): Int = (1 to number).product

  // exponential function -> e^x = 1 + x + x^2/2! + x^3/3! + x^4/4! ...
  def computeExponentialFunction(value: Double, terms: Int): Double =
    (1 to terms).foldLeft(1.0) { (result, term) => result + pow(value, term)/computeFactorial(term) }

  // f(x) = a1*x^b1 + a2*x^b2 + ... + an*x^bn
  // coefficientPair = (coefficient, index)
  def computeValueOfFunction(coefficients: List[Int], powers: List[Int], x: Double): Double = {
    coefficients.zipWithIndex.foldLeft(0.0) {
      (result: Double, coefficientPair: (Int, Int)) => result + coefficientPair._1*pow(x, powers(coefficientPair._2))
    }
  }
}
