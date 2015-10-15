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


  def foldRight[A, B](as: List[A], z:B)(f: (A, B) => B):B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
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
      case ys if n <= 0 => ys
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

  /**
   * Exercise 3.7
   * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters
   * a 0.0? Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
   * This is a deeper question that we’ll return to in chapter 5.
   */
  def product2(floats: List[Float]) =
    List.foldRight(floats, 1.0)(_ * _)

  /**
   * Exercise 3.9
   * Compute the length of a list using foldRight
   */
  def length[A](as: List[A]): Int =
    List.foldRight(as, 0)( (a, acc) => 1 + acc )

  /**
   * Exercise 3.10
   * Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for large lists
   * (we say it’s not stack-safe). Convince yourself that this is the case, and then write another general
   * list-recursion function, foldLeft, that is tail-recursive, using the techniques we discussed in the previous
   * chapter. Here is its signature:
   */
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, tail) => List.foldLeft(tail, f(z,h))(f)
    }

  /**
   * Exercise 3.11
   * Write sum, product, and a function to compute the length of a list using foldLeft.
   */
  def sum2(as: List[Int]):Int = List.foldLeft(as, 0)(_ + _)
  def product3(as: List[Float]):Float = List.foldLeft(as, 1f)((acc, a) => acc * a)
  def length2[A](as: List[A]):Int = List.foldLeft(as, 0)((acc, _) => acc + 1)


}