import scala.annotation.tailrec


def product(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 1
  else f(a) * product(f)(a + 1, b)
}

product(x => x)(1, 5)

def factorialCustom(number: Int): Int = {
  @tailrec
  def inner(number: Int, accumulated: Int): Int = {
    if(number == 0) accumulated else inner(number - 1, number * accumulated)
  }
  inner(5, 1)
}

def factorialCustomCurrying(number: Int): Int = {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if(a > b) 1
    else f(a) * product(f)(a + 1, b)
  }
  product(x => x)(1, number)
}

factorialCustom(5)
factorialCustomCurrying(5)