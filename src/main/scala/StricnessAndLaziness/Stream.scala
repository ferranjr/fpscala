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

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption


  /**
   * Exercise 5.13
   * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll.
   * The zipAll function should continue the traversal as long as either stream has more elements—it
   * uses Option to indicate whether each stream has been exhausted.
   */
  def mapWithUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this){
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeWithUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)){
      case (Cons(h, t),1) => Some((h(), (Stream.empty, 0)))
      case (Cons(h, t),n0) if n0 > 1 => Some((h(), (t(),n0-1)))
      case _ => None
    }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this){
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWithWithUnfold[B, C](s2: Stream[B])(f:(A, B) => C):Stream[C] =
    Stream.unfold((this, s2)){
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()),(t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold(this, s2){
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) =>
        Some((Some(h1()), None), (t1(), Stream.empty))
      case (Empty, Cons(h2, t2)) =>
        Some((None, Some(h2())), (Stream.empty, t2()))
      case _ => None
    }


  /**
   * Exercise 5.14
   * Hard: Implement startsWith using functions you’ve written. It should check if one Stream is a prefix of another.
   * For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
   */
  def startsWith[B](s: Stream[B]): Boolean =
    (this, s) match {
      case (_, Empty) => true
      case (Cons(h1,t1), Cons(h2,t2)) if h1() == h2() => t1().startsWith(t2())
      case _ => false
    }

  /**
   * Exercise 5.15
   * Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the input sequence,
   * starting with the original Stream. For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
   */
  def tails: Stream[Stream[A]] =
    Stream.unfold(this){
      case Empty => None
      case s:Stream[A] => Some((s, s drop 1))
    } append Stream(Empty)


  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  /**
   * Exercise 5.16
   * Hard: Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the
   * intermediate results. For example:
   *   scala> Stream(1,2,3).scanRight(0)(_ + _).toList
   *   res0: List[Int] = List(6,5,3,0)
   * This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0, 0). Your function should reuse intermediate results so that traversing a Stream with n elements always takes time linear in n. Can it be implemented using unfold? How, or why not? Could it be implemented using another function we’ve written?
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {

  def cons[A](h: => A, t: => Stream[A]):Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () =>tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))


  /**
   * Exercise 5.8
   * Generalize ones slightly to the function constant, which returns an infinite Stream
   * of a given value.
   */
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /**
   * Exercise 5.9
   * Write a function that generates an infinite stream of integers, starting from n,
   * then n + 1, n + 2, and so on.
   * In Scala, the Int type is a 32-bit signed integer, so this stream will switch from positive to
   * negative values at some point, and will repeat itself after about four billion elements.
   */
  def from(n: Int): Stream[Int] = {
    Cons(() => n, () => from(n + 1))
  }

  /**
   * Exercise 5.10
   * Write a function fibs that generates the infinite stream of
   * Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
   */
  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      Cons(() => f0, () => go(f1, f0 + f1))
    }
    go(0, 1)
  }

  /**
   * Exercise 5.11
   * Write a more general stream-building function called unfold. It takes an initial state, and a function
   * for producing both the next state and the next value in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
//    f(z) match {
//      case Some((h,s)) => cons(h, unfold(s)(f))
//      case None => empty
//    }
//    // or with fold
//    f(z).fold(empty[A])((p: (A,S)) => cons(p._1,unfold(p._2)(f)))
//    // or with map
    f(z).map((p: (A,S)) => cons(p._1,unfold(p._2)(f))).getOrElse(empty[A])

  /**
   * Exercise 5.12
   * Write fibs, from, constant, and ones in terms of unfold
   */
  val fibsWithUnfold: Stream[Int] =
    unfold((0,1)){ case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromWithUnfold(n: Int): Stream[Int] =
    unfold(n)( n => Some(n, n+1) )

  def constantWithUnfold[A](a: A): Stream[A] =
    unfold(a)( a => Some(a, a) )

}