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

  def sumOdd(arr: List[Int]): Int = arr.filter(_ % 2 != 0).sum

  def countNumberOfElements(arr: List[Int]): Int = arr.foldLeft(0) { (count, _) => count + 1}

  def getAbsoluteValues(arr: List[Int]): List[Int] = arr.map(_.abs)
}
