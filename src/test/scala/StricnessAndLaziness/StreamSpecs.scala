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
       Stream constant generator $e58
       Stream from generator $e59
       Stream fibbonacci generator $e510
       Stream unfold generator $e511
       Stream using unfold
          fibs $e512_1
          from $e512_2
          constant $e512_3
          map $e513_1
          take $e513_2
          takeWhile $e513_3
          zipWith $e513_4
          zipAll $e513_5
        Stream startWith $e514_1
        Stream startWith $e514_2
        Stream tails $e515
        Stream hasSubsequence ${hasSubsequenceTest && hasSubsequenceTest2}
     """

  def e51   = Stream( 1, 2, 3).toList must equalTo(List(1,2,3))

  def e52_1 = Stream(1,2,3,4,5,6).take(2).toList must equalTo(List(1,2))

  def e52_2 = Stream(1,2,3,4,5,6).drop(2).toList must equalTo(List(3,4,5,6))

  def e53   = Stream(1,2,3,4,5,6).takeWhile(_ <= 2).toList must equalTo(List(1,2))

  def e54_1 = Stream(1,2,3,4,5,6).forAll(_<=10) must equalTo(true)
  def e54_2 = Stream(1,2,3,4,5,6).forAll(_<=4) must equalTo(false)

  def e55   = Stream(1,2,3,4,5,6).takeWhileViaFoldRight(_ <= 2).toList must equalTo(List(1,2))

  def e56_1 = Stream(1,2,3).headOptionViaFold must equalTo(Some(1))
  def e56_2 = Stream().headOptionViaFold must equalTo(None)

  def e57_1 = Stream(1,2,3,4).map(_+1).toList must equalTo(List(2,3,4,5))
  def e57_2 = Stream(1,2,3,4).filter(_>2).toList must equalTo(List(3,4))
  def e57_3 = Stream(1,2).append(Stream(3,4)).toList must equalTo(List(1,2,3,4))
  def e57_4 = Stream(1,2).flatMap(Stream(_)).toList must equalTo(List(1,2))

  def e58   = Stream.constant(1).take(3).toList must equalTo(List(1,1,1))

  def e59   = Stream.from(1).take(3).toList must equalTo(List(1,2,3))

  def e510  = Stream.fibs.take(7).toList must equalTo(List(0,1,1,2,3,5,8))

  def e511  =
    Stream.unfold((0,1)){ case (f0,f1) => Some((f0,(f1,f0+f1))) }.take(7).toList must equalTo(List(0,1,1,2,3,5,8))

  def e512_1 = Stream.fibsWithUnfold.take(7).toList must equalTo(List(0,1,1,2,3,5,8))
  def e512_2 = Stream.fromWithUnfold(1).take(3).toList must equalTo(List(1,2,3))
  def e512_3 = Stream.constantWithUnfold(1).take(3).toList must equalTo(List(1,1,1))

  def e513_1 = Stream(1,2,3,4).mapWithUnfold(_+1).toList must equalTo(List(2,3,4,5))
  def e513_2 = Stream(1,2,3,4,5,6).takeWithUnfold(2).toList must equalTo(List(1,2))
  def e513_3 = Stream(1,2,3,4,5,6).takeWhileWithUnfold(_ <= 2).toList must equalTo(List(1,2))
  def e513_4 = Stream(1,2,3).zipWithWithUnfold(Stream(1,2,3))(_+_).toList must equalTo(List(2,4,6))
  def e513_5 = Stream(1,2).zipAll(Stream(1,2,3)).toList must equalTo(List((Some(1), Some(1)),(Some(2), Some(2)), (None, Some(3))))

  def e514_1 = Stream(1,2,3,4,5).startsWith(Stream(1,2,3)) must equalTo(true)
  def e514_2 = Stream(1,2,3,4,5).startsWith(Stream(5,2,3)) must equalTo(false)

  def e515 = Stream(1,2,3).tails.toList.map(_.toList) must equalTo(List(List(1,2,3),List(2,3),List(3),List()))

  def hasSubsequenceTest = Stream(1,2,3,4,5).hasSubsequence(Stream(3,4,5)) must equalTo(true)
  def hasSubsequenceTest2 = Stream(1,2,3,5).hasSubsequence(Stream(3,4,5)) must equalTo(false)

  def e516 = Stream(1,2,3).scanRight(0)(_ + _).toList must equalTo(List(6,5,3,0))
}
