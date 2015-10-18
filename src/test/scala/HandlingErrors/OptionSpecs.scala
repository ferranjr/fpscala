package HandlingErrors

import org.specs2._


class OptionSpecs extends Specification {

  def is =
    s2"""
        | Option implementing functions:
        |   map works $e41_1
        |   flatMap works $e41_2
        |   getOrElse works $e41_3
        |   orElse works $e41_4
        |   filter works ${e41_5a && e41_5b}
        |
        |   Mean $e42a
        |   Variance $e42b
        |
        |   map2 rules $e43
        |   sequence $e44
        |
      """.stripMargin

  def e41_1 = Some(4).map(_ + 1) must equalTo(Some(5))

  def e41_2 = Some(4).flatMap( Some(_) ) must equalTo(Some(4))

  def e41_3 = None.getOrElse(4) must equalTo(4)

  def e41_4 = None.orElse(Some(4)) must equalTo(Some(4))

  def e41_5a = Some(4).filter( _ > 5 ) must equalTo(None)
  def e41_5b = Some(4).filter( _ < 5 ) must equalTo(Some(4))


  def e42a = Option.mean(Seq[Double](600, 470, 170, 430, 300)) must equalTo(Some(394))
  def e42b = Option.variance(Seq[Double](600, 470, 170, 430, 300)) must equalTo(Some(21704))

  def e43 = Option.map2(Some(3), Some(3))( _ * _ ) must equalTo(Some(9))

  def e44 = Option.sequence(List(Some(3), Some(4), Some(5))) must equalTo(Some(List(3,4,5)))

}
