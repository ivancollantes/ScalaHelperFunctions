import scala.annotation.tailrec

def factorial(number: Int): Int = {
  @tailrec
  def innerFactorial(number: Int, accumulated: Int): Int = {
    if(number == 0) accumulated else innerFactorial(number - 1, number * accumulated)
  }
  innerFactorial(5, 1)
}

factorial(5)
