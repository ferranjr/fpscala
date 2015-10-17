package FunctionalDataStructures

import org.specs2._


class ListSpecs extends Specification {

  def is = s2"""

    Functional Data Structures Specs

      List sum works $sumTest
      List product works $prodTest
      List tail works $e32
      List set Head works $e33
      List drop works $e34
      List dropWhile works $e35
      List init works $e36
      List product with foldRight works $e37
      List length with foldRight works $e39
      List sum with foldLeft works $e311_1
      List product with foldLeft works $e311_2
      List length with foldLeft works $e311_3
      List reverse with fold works $e312
      List append with fold works $e314
      List concat with fold works $e315
      List addOne works $e316
      List tooStrings works $e317
      List map works $e318_1
      List map stack safe works $e318_2
      List filter works $e319
      List flatMap works $e320
      List filter via flatMap works $e321
      List addPariwise works $e322
      List zipWith works $e323
      List startsWith works $e324h
      List hasSubsequence works $e324

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

  // Exercise 3.5
  def e35 = List.dropWhile(List("foo","bar","baz","foobar"), (x:String) => x == "baz" ) must equalTo(List("foobar"))

  // Exercise 3.6
  def e36 = List.init(List('a', 'b', 'c', 'd')) must equalTo(List('a', 'b', 'c'))

  // Exercise 3.7
  def e37 = List.product2(List(1,2,3)) must equalTo(6)

  // Exercise 3.9
  def e39 = List.length(List(1,2,3,4,5)) must equalTo(5)

  // Exercise 3.11 (and 3.10)
  def e311_1 = List.sum2(List(1,2,3,4)) must equalTo(10)
  def e311_2 = List.product3(List(1,2,3,4)) must equalTo(24)
  def e311_3 = List.length2(List(1,2,3,4,5)) must equalTo(5)

  // Exercise 3.12
  def e312 = List.reverse(List(1,2,3,4,5)) must equalTo(List(5,4,3,2,1))

  // Exercise 3.14
  def e314 = List.append2(List(1,2,3), List(4,5,6,7)) must equalTo(List(1,2,3,4,5,6,7))

  // Exercise 3.15
  def e315 = List.concat(List(List(1,2,3),List(4,5,6),List(7))) must equalTo(List(1,2,3,4,5,6,7))

  // Exercise 3.16
  def e316 = List.addOne(List(1,2,3,4,5)) must equalTo(List(2,3,4,5,6))

  // Exercise 3.17
  def e317 = List.toStrings(List(1,2,3,4,5)) must equalTo(List("1.0","2.0","3.0","4.0","5.0"))

  // Exercise 3.18
  def e318_1 = List.map(List(1,2,3))(_ + 1) must equalTo(List(2,3,4))
  def e318_2 = List.map_2(List(1,2,3))(_ + 1) must equalTo(List(2,3,4))

  // Exercise 3.19
  def e319 = List.filter(List(1,2,3,4,5,6,7,8))( _ % 2 == 0 ) must equalTo(List(2,4,6,8))

  // Exercise 3.20
  def e320 = List.flatMap(List(1,2,3))(i => List(i,i)) must equalTo(List(1,1,2,2,3,3))

  // Exercise 3.21
  def e321 = List.filterViaFlatMap(List(1,2,3,4,5,6,7,8))( _ % 2 == 0 ) must equalTo(List(2,4,6,8))

  // Exercise 3.22
  def e322 = List.addPairwise(List(1,2,3), List(4,5,6)) must equalTo(List(5,7,9))

  // Exercise 3.23
  def e323 = List.zipWith(List(1,2,3), List(4,5,6))(_+_) must equalTo(List(5,7,9))

  // Exercise 3.24
  def e324 = List.hasSubsequence(List(1,2,3,4,5,6), List(3,4,5)) must equalTo(true)
  // helper 3.24
  def e324h = List.startsWith(List(1,2,3,4,5), List(1,2,3)) must equalTo(true)


}
