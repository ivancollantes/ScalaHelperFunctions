import scala.annotation.tailrec
import scala.math._

object HelperFunctions {

  def replicateNumber(number: Int, repetitions: Int): List[Int] = List.fill(repetitions)(number)

  def listReplication(num: Int, arr: List[Int]): List[Int] = arr.flatMap(replicateNumber(_, num))

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

  def computeFactorial(x: Int): Int = (1 to x).product

  /*
   * Exponential function -> f(x) = e^x = 1 + x + x^2/2! + x^3/3! + x^4/4! ...
   */
  def computeExponentialFunction(x: Double, terms: Int): Double =
    (1 to terms).foldLeft(1.0) { (result, term) => result + pow(x, term)/computeFactorial(term) }

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
  def computeAreaUnderFunction(f: (List[Int], List[Int], Double) => Double,
                               upperLimit: Int,
                               lowerLimit: Int,
                               coefficients: List[Int],
                               powers: List[Int],
                               fixedSubIntervalLength: Double): Double = {
    val n: Int = (upperLimit - lowerLimit)/fixedSubIntervalLength.toInt
    (1 to n).foldLeft(0.0) { (result: Double, i: Int) => {
      val deltaXi: Double = (upperLimit - lowerLimit)/n
      val Ci: Double = lowerLimit + deltaXi*i
      result + f(coefficients, powers, Ci)*deltaXi
    }}
  }

  /******************************************************
   * Functional Programming Principles in Scala, week 1
   */
  def sqrtCustom(x: Double) = {
    def isGoodEnough(guess: Double, x: Double) = abs(guess * guess - x)/x < 0.001
    def improve(guess: Double, x: Double) = (guess + x / guess) / 2
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)
    sqrtIter(1.0, x)
  }

  def absCustom(x: Double) = if (x < 0) -x else x

  def factorialCustom(number: Int): Int = {
    @tailrec
    def inner(number: Int, accumulated: Int): Int = {
      if(number == 0) accumulated else inner(number - 1, number * accumulated)
    }
    inner(number, 1)
  }

  def getNumberInPascalTrianglePosition(column: Int, row: Int): Int =
    if(column == 0 || column == row) 1
    else getNumberInPascalTrianglePosition(column - 1, row - 1) + getNumberInPascalTrianglePosition(column, row - 1)

  def isThereParenthesisBalance(chars: List[Char]): Boolean = {

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
  /* AKA sum */
  def sumResultOfApplyingFunctionToNumbersFromAToB(f: Int => Int)(a: Int, b: Int): Int = {
    @tailrec
    def inner(a: Int, acc: Int): Int = {
      if (a > b) acc
      else inner(a + 1, acc + f(a))
    }
    inner(a, 0)
  }

  /* AKA product */
  def multiplyResultOfApplyingFunctionToNumbersFromAToB(f: Int => Int)(a: Int, b: Int): Int = {
    if(a > b) 1
    else f(a) * multiplyResultOfApplyingFunctionToNumbersFromAToB(f)(a + 1, b)
  }

  def factorialCustom2(number: Int): Int = multiplyResultOfApplyingFunctionToNumbersFromAToB(x => x)(1, number)

  /*
   * f: function to apply to each number
   * combine: function with which combine the results of f
   * zero: initial value for variable in mapReduce in which the result is going to be accumulated:
   *       1 for product
   *       0 for sum
   */
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if(a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }

  def productWithMapReduce(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
  def sumWithMapReduce(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)
  def factorialWithMapReduce(x: Int): Int = mapReduce(x => x, (x, y) => x * y, 1)(1, x)

  def getFixedPointOfFunction(f: Double => Double)(firstGuess: Double): Double = {
    val tolerance: Double = 0.0001
    def isCloseEnough(x: Double, y: Double) = abs((x-y)/x)/x < tolerance
    @tailrec
    def inner(guess: Double): Double = {
      val next = f(guess)
      if(isCloseEnough(guess, next)) next
      else inner(next)
    }
    inner(firstGuess)
  }







  def lastCustom[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => lastCustom(ys)
  }

  def initCustom[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: initCustom(ys)
  }

  def concatCustom[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concatCustom(xs, zs)
  }

  def sortCustom[T](xs: List[T])(implicit order: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if(n == 0) xs
    else {
      def mergeCustomWithPairs[T](xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xst, y :: yst) =>
          if(order.lt(x, y)) x :: mergeCustomWithPairs(xst, ys)
          else y :: mergeCustomWithPairs(xs, yst)
      }
      val (fst, snd) = xs splitAt n
      mergeCustomWithPairs(sortCustom(fst), sortCustom(snd))
    }
  }

  def reverseCustom[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverseCustom(ys) ++ List(y)
  }

  def removeAtCustom[T](n: Int, xs: List[T]): List[T] = (xs take n) ++ (xs drop n + 1)

  /* Takes a list of elements and packs them into a list of lists of equal elements */
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xt => {
      val (head: List[T], tail: List[T]) = xs.partition(_ == x)
      head :: pack(tail)
    }
  }

  /* Takes a list of elements and packs them into a list of pairs conformed by the element and the number of repetitions */
  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map (l => (l.head, l.length))

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = (xs zip ys).map{ case (x, y) => x * y }.sum
  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double = (for((x,y) <- xs zip ys) yield x * y).sum

  def isPrime(n: Int): Boolean = (2 until n).forall(d => n%d != 0)

  def queens(n: Int): Set[List[Int]] = {
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length
      val queensWithRow: Seq[(Int, Int)] = (row - 1 to 0 by -1) zip queens
      queensWithRow forall {
        case (r, c) => col != c && math.abs(col - c) != row - r
      }
    }
    def placeQueens(k: Int): Set[List[Int]] = {
      if(k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }
    placeQueens(n)
  }
}
