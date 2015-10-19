package HandlingErrors


sealed trait Either[+E,+A]{

  /**
   * Exercise 4.6
   * Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
   */

  def map[B](f: A => B): Either[E, B] =
    this match {
      case error@Left(_)  => error
      case Right(v)       => Right(f(v))
    }


  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case error@Left(_)  => error
      case Right(v)       => f(v)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case error@Left(_)  => b
      case ok@Right(_)    => ok
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
    for{
      a <- this
      b1 <- b
    } yield f(a, b1)

}
case class Left[E](value: E) extends Either[E, Nothing]
case class Right[A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  /**
   * Exercise 4.7
   * Implement sequence and traverse for Either.
   * These should return the first error that’s encountered, if there is one.
   */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)( x => x )

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

}