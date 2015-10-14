import org.specs2._

class FunctionalDataStructuresSpecs extends Specification {

  import FunctionalDataStructures.List

  def is = s2"""

   Functional Data Structures Specs

      List sum works $sumTest
      List product works $prodTest
      List tail works $e32
      List set Head works $e33
      List drop works $e34

    """


  // Basic list operations
  def sumTest = List.sum(List(1,2,3,4,5))   must equalTo(15)
  def prodTest = List.product(List(1,2,3))  must equalTo(6)


  // Exercise 3.2
  def e32 = List.tail( List(1,2,3,4,5) ) must equalTo(List(2,3,4,5))

  // Exercise 3.3
  def e33 = List.setHead(List(1,2,3,4,5), 9) must equalTo(List(9,1,2,3,4,5))

  // Exercise 3.4
  def e34 = List.drop(List(1,2,3,4,5), 3) must equalTo(List(4,5))

}
