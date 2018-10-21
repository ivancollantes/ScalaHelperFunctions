import scala.annotation.tailrec
import scala.math._

object HelperFunctions {

  def replicateNumber(number:Int, repetitions:Int): List[Int] = List.fill(repetitions)(number)

  def listReplication(num:Int, arr:List[Int]): List[Int] = arr.flatMap(replicateNumber(_, num))

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

  /*
   * exponential function -> e^x = 1 + x + x^2/2! + x^3/3! + x^4/4! ...
   */
  def computeExponentialFunction(value: Double, terms: Int): Double =
    (1 to terms).foldLeft(1.0) { (result, term) => result + pow(value, term)/computeFactorial(term) }

  /*
   * f(x) = a1*x^b1 + a2*x^b2 + ... + an*x^bn
   * coefficientPair = (coefficient, index)
   */
  def computeValueOfFunction(coefficients: List[Int], powers: List[Int], x: Double): Double = {
    coefficients.zipWithIndex.foldLeft(0.0) {
      (result: Double, coefficientPair: (Int, Int)) => result + coefficientPair._1*pow(x, powers(coefficientPair._2))
    }
  }

  def computeAreaOfCircleObtainedRotatingValueOfFunction(coefficients: List[Int], powers: List[Int], x: Double): Double = {
    Pi*pow(computeValueOfFunction(coefficients, powers, x), 2)
  }

  /*
   * https://www.math.ucdavis.edu/~kouba/CalcTwoDIRECTORY/defintdirectory/
   * to use with computeValueOfFunction or computeAreaOfCircleObtainedRotatingValueOfFunction as "func"
   */
  def computeAreaUnderFunction(func: (List[Int], List[Int], Double)=>Double,
                               upperLimit: Int,
                               lowerLimit: Int,
                               coefficients: List[Int],
                               powers: List[Int],
                               fixedSubIntervalLength: Double): Double = {
    val n: Int = (upperLimit - lowerLimit)/fixedSubIntervalLength.toInt
    (1 to n).foldLeft(0.0) { (result: Double, i: Int) => {
      val deltaXi: Double = (upperLimit - lowerLimit)/n
      val Ci: Double = lowerLimit + deltaXi*i
      result + func(coefficients, powers, Ci)*deltaXi
    }}
  }

  def sqrtCustom(x: Double) = {
    def isGoodEnough(guess: Double, x: Double) = abs(guess * guess - x)/x < 0.01
    def improve(guess: Double, x: Double) =(guess + x / guess) / 2
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)
    sqrtIter(1.0, x)
  }

  def absCustom(x:Double) = if (x < 0) -x else x

  def factorialCustom(number: Int): Int = {
    @tailrec
    def inner(number: Int, accumulated: Int): Int = {
      if(number == 0) accumulated else inner(number - 1, number * accumulated)
    }
    inner(5, 1)
  }

  /******************************************************
   * Functional Programming Principles in Scala, week 1
   */
  def getNumberInPascalTrianglePosition(column: Int, row: Int): Int =
    if(column == 0 || column == row) 1
    else getNumberInPascalTrianglePosition(column - 1, row - 1) + getNumberInPascalTrianglePosition(column, row - 1)

  def isThereBracketBalance(chars: List[Char]): Boolean = {

    @tailrec
    def inner(remainingChars: List[Char], accumulator: Int): Boolean = {

      def updateAccumulator(char: Char, accumulator: Int): Int =
        if(char == '(') accumulator + 1
        else if (char == ')') accumulator - 1
        else accumulator

      val currentAccumulator: Int = updateAccumulator(remainingChars.head, accumulator)

      val balance: Boolean = if(currentAccumulator == 0) true else false

      if(currentAccumulator < 0) false
      else if (remainingChars.tail.isEmpty) balance
      else inner(remainingChars.tail, currentAccumulator )
    }

    inner(chars, 0)
  }

  def getNumberOfWaysToGiveChange(money: Int, coins: List[Int]): Int = {

    def inner(coins: List[Int], numberOfCoins: Int, money: Int): Int = {
      if (money < 0) 0
      else if (money == 0) 1
      else if (numberOfCoins == 0) 0
      else inner(coins, numberOfCoins - 1, money) +
           inner(coins, numberOfCoins, money - coins(numberOfCoins-1))
    }

    inner(coins, coins.length, money)
  }
  /*******************************************************/

  /*******************************************************
    * Functional Programming Principles in Scala, week 2
    */
  def sumResultOfApplyingFunctionToNumbersFromAToB(f: Int => Int)(a: Int, b: Int): Int = {
    @tailrec
    def inner(a: Int, acc: Int): Int = {
      if (a > b) acc
      else inner(a + 1, acc + f(a))
    }
    inner(a, 0)
  }

  def multiplyResultOfApplyingFunctionToNumbersFromAToB(f: Int => Int)(a: Int, b: Int): Int = {
    if(a > b) 1
    else f(a) * multiplyResultOfApplyingFunctionToNumbersFromAToB(f)(a + 1, b)
  }

  def factorialCustom2(number: Int): Int = multiplyResultOfApplyingFunctionToNumbersFromAToB(x => x)(1, number)
}
