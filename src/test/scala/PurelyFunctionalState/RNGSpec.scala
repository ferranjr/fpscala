package PurelyFunctionalState

import PurelyFunctionalState.RNG.SimpleRNG
import org.specs2._

class RNGSpec extends Specification {

  import PurelyFunctionalState.RNG

  def is = s2"""
      Random Generator
        nonNegative $e61
        double      $e62
        ints        $e64
        nonNegativeEven $nonNegEven
        double with map $e65

      RollDie
        should not give us a 0 $rollDie
    """

  def e61 = RNG.nonNegativeInt(SimpleRNG(42))._1 must equalTo(16159453)
  def e62 = RNG.double(SimpleRNG(42))._1 must equalTo(0.007524831686168909)

  def e64 = RNG.ints(3)(SimpleRNG(42))._1.length must equalTo(3)

  def nonNegEven = RNG.nonNegativeEven(SimpleRNG(1))._1 must equalTo(384748)

  def e65 = RNG.double2(SimpleRNG(42))._1 must equalTo(0.007524831686168909)

  def rollDie = RNG.rollDie(SimpleRNG(5))._1 must equalTo(1)

}
