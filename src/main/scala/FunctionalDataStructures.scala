package FunctionalDataStructures


/**
 * Creating a List
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]


object List {

  def sum(ints: List[Int]): Int =
    ints match {
      case Nil          => 0
      case Cons(x,xs)   => x + sum(xs)
    }

  def product(floats: List[Double]):Double=
    floats match {
      case Nil          => 1.0
      case Cons(0, _)   => 0.0
      case Cons(x, xs)  => x * product(xs)
    }

  /**
   * Variadic function to allow list creation from one to n elements using List(1,2,3)
   * @param as
   * @tparam A
   * @return
   */
  def apply[A](as: A*):List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail :_*) )
  }


  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(x, xs) => Cons(x, append(xs, a2))
    }


  /**
   * Exercise 3.2
   * Implement the function tail for removing the first element of a List.
   * Note that the function takes constant time. What are different choices you could make
   * in your implementation if the List is Nil? We’ll return to this question in the next chapter.
   */
  def tail[A](l: List[A]):List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  /**
   * Exercise 3.3
   * Using the same idea, implement the function setHead for replacing the first element
   * of a List with a different value.
   */
  def setHead[A](l: List[A], a: A):List[A] =
    l match {
      case Nil => Cons( a, Nil)
      case ys@Cons(x, xs) => Cons(a, ys)
    }

  /**
   * Exercise 3.4
   * Generalize tail to the function drop, which removes the first n elements from a list. Note that
   * this function takes time proportional only to the number of elements being dropped—we don’t need
   * to make a copy of the entire List.
   */
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => Nil
      case ys@Cons(_, _) if n == 0 => ys
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  /**
   * Exercise 3.5
   * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
   */
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => xs
      case Cons(x,xs) => dropWhile(xs,f)
    }


  /**
   * Exercise 3.6
   * Not everything works out so nicely. Implement a function, init, that returns a List consisting of all
   * but the last element of a List. So, given List(1,2,3,4), init will return List(1,2,3).
   * Why can’t this function be implemented in constant time like tail?
   */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
      case Cons(x, xs) => Cons(x, init(xs))
    }

}