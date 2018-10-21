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
  inner(number, 1)
}

def factorialCustomCurrying(number: Int): Int = {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if(a > b) 1
    else f(a) * product(f)(a + 1, b)
  }
  product(x => x)(1, number)
}

factorialCustom(7)
factorialCustomCurrying(5)

def general(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 1
  else f(a) * general(f)(a + 1, b)
}

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if(a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

def productWithMapReduce(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
def factorialWithMapReduce(number: Int): Int = mapReduce(x => x, (x, y) => x * y, 1)(1, number)

factorialCustom(7)
factorialWithMapReduce(7)