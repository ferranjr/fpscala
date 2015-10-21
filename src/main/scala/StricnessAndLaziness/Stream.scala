package StricnessAndLaziness


sealed trait Stream[+A]{

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h()) // We must explicitly force the thunk using h()
  }

  /**
   * Exercise 5.1
   * Write a function to convert a Stream to a List, which will force its evaluation and let you look at
   * it in the REPL. You can convert to the regular List type in the standard library. You can place this
   * and other functions that operate on a Stream inside the Stream trait.
   */
  def toList: List[A] = {
    //  //this would have a stack overflow for long streams
    //    this match {
    //      case Empty => Nil
    //      case Cons(h, t) => h() :: t().toList
    //    }
    // Alternatively
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]):List[A] = {
      s match {
        case Cons(h, tail) => go(tail(), h() :: acc )
        case _ => acc
      }
    }
    go(this, List()).reverse
  }


  /**
   * Exercise 5.2
   * Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first
   * n elements of a Stream.
   */
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, tail) if n > 1 => Stream.cons(h(), tail().take(n - 1))
      case Cons(h, tail) if n == 1 => Stream.cons(h(), Empty)
      case _ => Empty
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, tail) if n > 0 => tail().drop(n-1)
      case _ => this
    }

  /**
   * Exercise 5.3
   * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
   */
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, tail) if p(h()) => Stream.cons(h(), tail().takeWhile(p))
      case _ => Empty
    }


  def exists(p: A => Boolean): Boolean =
//    this match {
//      case Cons(h, t) => p(h()) || t().exists(p)
//      case _ => false
//    }
    foldRight(false)((a,b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B ): B =
    this match {
      case Cons(h,t)  => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  /**
   * Exercise 5.4
   * Implement forAll, which checks that all elements in the Stream match a given predicate. Your implementation
   * should terminate the traversal as soon as it encounters a nonmatching value.
   */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  /**
   * Exercise 5.5
   * Use foldRight to implement takeWhile.
   */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a,b) =>
      if(p(a)) Stream.cons(a, b)
      else Empty
    )

  /**
   * Exercise 5.6
   * Hard: Implement headOption using foldRight.
   */
  def headOptionViaFold: Option[A] =
    foldRight(None:Option[A])(
      (a, _) => Some(a)
    )

  /**
   * Exercise 5.7
   * Implement map, filter, append, and flatMap using foldRight.
   * The append method should be non-strict in its argument.
   */
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream[B]())((h,t) => Stream.cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream[A]()){ (h,t) =>
      if(f(h)) Stream.cons(h, t)
      else     t
    }

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s){ (h,t) => Stream.cons(h, t) }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]()){ (h,t) =>
      f(h) append t
    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {

  def cons[A](h: => A, t: => Stream[A]):Stream[A] = {
    val head = h
    val tail = t
    Cons(() => head, () =>tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))

}