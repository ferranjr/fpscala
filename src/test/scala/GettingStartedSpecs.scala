import org.specs2._

class GettingStartedSpecs extends Specification {

  import GettingStarted._

  def is =
     s2"""
      FP in Scala GettingStarted Specification

        Factorial ${fact1 && fact2 && fact3}

        2.1 Fibonacci ${fib1 && fib2 && fib3}

        isSorted Int  $sort1
        isSorted Char $sort2

     """

  // Factorial tests
  def fact1 = factorial(0) must equalTo(1)
  def fact2 = factorial(1) must equalTo(1)
  def fact3 = factorial(5) must equalTo(120)

  // Fibonacci tests
  def fib1  = fib(0) must equalTo(0)
  def fib2  = fib(1) must equalTo(1)
  def fib3  = fib(5) must equalTo(5)

  // isSorted
  def sort1 = isSorted(Array(1,2,3,4,5,6), (x:Int, y:Int) => x > y ) must beTrue
  def sort2 = isSorted(Array('a','b','c'), (x:Char, y:Char ) => x > y ) must beTrue

}
