package StricnessAndLaziness

import org.specs2._


class StreamSpecs extends Specification {

  def is =
    s2"""
         Stream toList works $e51
         Stream take works $e52_1
         Stream drop works $e52_2
         Stream takeWhile works $e53
         Stream forAll works ${e54_1 && e54_2}
         Stream takeWhile works $e55
         Stream headOption via foldRight works ${e56_1 && e56_2}
         Stream map works $e57_1
         Stream filter works $e57_2
         Stream append works $e57_3
         Stream flatMap works $e57_4
     """

  def e51 = Stream( 1, 2, 3).toList must equalTo(List(1,2,3))

  def e52_1 = Stream(1,2,3,4,5,6).take(2).toList must equalTo(List(1,2))

  def e52_2 = Stream(1,2,3,4,5,6).drop(2).toList must equalTo(List(3,4,5,6))

  def e53 = Stream(1,2,3,4,5,6).takeWhile(_ <= 2).toList must equalTo(List(1,2))

  def e54_1 = Stream(1,2,3,4,5,6).forAll(_<=10) must equalTo(true)
  def e54_2 = Stream(1,2,3,4,5,6).forAll(_<=4) must equalTo(false)

  def e55 = Stream(1,2,3,4,5,6).takeWhileViaFoldRight(_ <= 2).toList must equalTo(List(1,2))

  def e56_1 = Stream(1,2,3).headOptionViaFold must equalTo(Some(1))
  def e56_2 = Stream().headOptionViaFold must equalTo(None)

  def e57_1 = Stream(1,2,3,4).map(_+1).toList must equalTo(List(2,3,4,5))
  def e57_2 = Stream(1,2,3,4).filter(_>2).toList must equalTo(List(3,4))
  def e57_3 = Stream(1,2).append(Stream(3,4)).toList must equalTo(List(1,2,3,4))
  def e57_4 = Stream(1,2).flatMap(Stream(_)).toList must equalTo(List(1,2))

}
