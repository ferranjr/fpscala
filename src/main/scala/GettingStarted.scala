/**
 * Some Exercises from FP in Scala
 *
 * Chapter 2
 * ~~~~~~~~~~
 *
 */

object GettingStarted {

  /**
   * Implementing factorial in tail recursive mode
   * @param n value to get the factorial for
   * @return
   */
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int): Int = {
      if(n <= 0) acc
      else       loop(n-1, n * acc)
    }

    loop(n, 1)
  }

  /**
   * Exercise 2.1
   * ~~~~~
   * Recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
   * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum
   * of the previous two --- the sequence begins 0, 1, 1, 2, 3, 5. Your definition
   * should use a local tail-recursive function
   */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else        loop(n - 1, cur, prev + cur)

    loop(n, 0, 1)
  }

  /**
   * Exercise 2.2
   * ~~~~~~
   * Implement isSorted, which checks wether an Array[A] is sorted according to a
   * given comparison function.
   *
   * @param as Array to be ordered
   * @param gt function to compare
   * @tparam A Type wrapped of the elements within the array
   * @return if the it is ordered or not
   */
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int) : Boolean = {
      if( n >= as.length-1) true
      else if (gt( as(n), as(n+1) )) false
      else go(n + 1)
    }

    go(0)
  }


  /**
   * Exercise 2.3
   * ~~~~~~~
   * Let's look at another example, currying, which converts a function f of two arguments
   * into a function of one argument that partially applies f. Here again there´ only one
   * implementation that compiles. Write this implementation
   */
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    a => b => f(a, b)


  /**
   * Exercise 2.4
   * ~~~~~~~~
   * Implement uncurry, which reverses the transformation of curry. Note that since => associates
   * to the right, A => (B => C) can be written as A => B => C.
   */
  def uncurry[A,B,C](f: A => B => C ): (A, B) => C =
    (a, b) => f(a)(b)


  /**
   * Exercise 2.5
   * ~~~~~~~
   * Implement the higher-order function that composes two functions.
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))


}
