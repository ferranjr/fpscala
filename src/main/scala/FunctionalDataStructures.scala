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


  /**
   * Exercise 3.12
   * Write a function that returns the reverse of a list (given List(1,2,3)
   * it returns List(3,2,1)). See if you can write it using a fold.
   */
  def reverse[A](as: List[A]) =
    List.foldLeft(as, List[A]())( (acc, a) => Cons(a,acc))


  /**
   * Exercise 3.13
   * Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
   * Implementing foldRight via foldLeft is useful because it lets us implement foldRight
   * tail-recursively, which means it works even for large lists without overflowing the
   * stack.
   */
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    List.foldLeft(reverse(as),z)((a,b) => f(b,a))

  def foldLeft2[A,B](as: List[A], z: B)(f: (B,A) => B ):B =
    List.foldRight(as, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  /**
   * Exercise 3.14
   * Implement append in terms of either foldLeft or foldRight
   */
  def append2[A](l: List[A], r: List[A]): List[A] =
    List.foldRight(l,r)(Cons(_,_))

  /**
   * Exercise 3.15
   * Hard: Write a function that concatenates a list of lists into a single list. Its
   * runtime should be linear in the total length of all lists. Try to use functions we
   * have already defined.
   */
  def concat[A](ls: List[List[A]]):List[A] =
    List.foldRight(ls,Nil:List[A])(append)

  /**
   * Exercise 3.16
   * Write a function that transforms a list of integers by adding 1 to each element
   * (Reminder: this should be a pure function that returns a new List!)
   */
  def addOne(l: List[Int]):List[Int] =
    List.foldRight(l, List[Int]())((a,b) => Cons(a+1, b))

  /**
   * Exercise 3.17
   * Write a function that turns each value in a List[Double] into a String. You can use
   * the expression d.toString to convert some d: Double to a String.
   */
  def toStrings(l: List[Double]):List[String] =
    List.foldRight(l, List[String]())((a,b) => Cons(a.toString, b))

  /**
   * Exercise 3.18
   * Write a function map that generalizes modifying each element in a list while maintaining
   * the structure of the list.
   * Here is its signature:[12] 12 In the standard library, map and flatMap are methods of List.
   */

  // Implementation straight forward but not stack safe
  def map[A,B](as: List[A])(f: A => B): List[B] =
    List.foldRight(as, List[B]())((a,b) => Cons(f(a), b))

  // Implementing based on the stack safe foldRight
  def map_2[A,B](as: List[A])(f: A => B): List[B] =
    List.foldRight2(as, List[B]())((a,b) => Cons(f(a), b))


  /**
   * Exercise 3.19
   * Write a function filter that removes elements from a list unless they satisfy a given
   * predicate. Use it to remove all odd numbers from a List[Int].
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    List.foldRight(as, Nil:List[A]){ (a,b) =>
      if(f(a)) Cons(a,b)
      else b
    }

  /**
   * Exercise 3.20
   * Write a function flatMap that works like map except that the function given will return a list
   * instead of a single result, and that list should be inserted into the final resulting list.
   * For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
   */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    List.foldRight2(as, List[B]())((a,acc) => List.append(f(a), acc))
//    List.concat(map(as)(f))

  /**
   * Exercise 3.21
   * Use flatMap to implement filter
   */
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    List.flatMap(as){ a =>
      if(f(a)) List(a)
      else Nil
    }

  /**
   * Exercise 3.22
   * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
   * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
   */
  def addPairwise(as: List[Int], bs: List[Int]):List[Int] =
    (as,bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, tail1), Cons(h2, tail2)) =>
        Cons(h1 + h2, addPairwise(tail1, tail2))
    }

  /**
   * Exercise 3.23
   * Generalize the function you just wrote so that it’s not specific to integers or addition.
   * Name your generalized function zipWith.
   */
  def zipWith[A, B, C](as: List[A], bs: List[B])(f:(A, B) => C):List[C] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, tail1), Cons(h2, tail2)) =>
        Cons(f(h1, h2), zipWith(tail1, tail2)(f))
    }


}