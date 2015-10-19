package HandlingErrors

import org.specs2._


class EitherSpecs extends Specification {

  def is =
    s2"""
       Building Either right biased version

        map works ${e46_1a && e46_1b}
        flatMap works ${e46_2a && e46_2b}
        orElse works ${e46_3a && e46_3b}
        map2 works $e46_4

        sequence works ${e47_1a && e47_1b}
        traverse works ${e47_2a && e47_2b}
    """

  // Exercise 4.6

  def e46_1a = Right(4).map(_ + 1) must equalTo(Right(5))
  def e46_1b = ( Left("WHAT"):Either[String, Int] ).map(_ + 1) must equalTo(Left("WHAT"))

  def e46_2a = Right(4).flatMap( a => Right(a + 1) ) must equalTo(Right(5))
  def e46_2b = ( Left("WHAT"):Either[String, Int] ).flatMap( a => Right(a + 1) )  must equalTo(Left("WHAT"))

  def e46_3a = Right(4).orElse( Right(1) ) must equalTo(Right(4))
  def e46_3b = ( Left("WHAT"):Either[String, Int] ).orElse( Right(1) )  must equalTo(Right(1))

  def e46_4 = Right(4).map2(Right(2))( _ + _ ) must equalTo(Right(6))


  // Exercise 4.7
  def e47_1a = Either.sequence(List(Right(4), Right(5), Right(7))) must equalTo(Right(List(4,5,7)))
  def e47_1b = Either.sequence(List(Right(4), Left("SHIT"), Right(8))) must equalTo(Left("SHIT"))

  def e47_2a = Either.traverse(List("1","2"))( a =>  Either.Try(a.toInt) ) must equalTo(Right(List(1,2)))
  def e47_2b = Either.traverse(List("1","2", "foo"))( a =>  Either.Try(a.toInt) ) must beAnInstanceOf[Left[Exception]]

}
