package FunctionalDataStructures

import org.specs2.Specification


class TreeSpecs extends Specification {

  def is = s2"""

    Tree size works $e325
    Tree maximum works $e326
    Tree detph works $e327
    Tree map works $e328

    Tree fold works
      size with fold $e329_1
      maximum with fold $e329_2
      depth with fold $e329_3
      map with fold $e329_4

  """

  val testTree = Branch(Branch(Leaf(1), Leaf(5)), Branch(Branch(Branch(Leaf(1),Leaf(2)),Leaf(4)), Leaf(2)))

  def e325 = Tree.size(testTree) must equalTo(11)

  def e326 = Tree.maximum(testTree) must equalTo(5)

  def e327 = Tree.depth(testTree) must equalTo(4)

  def e328 = Tree.map(testTree)(_ + 1) must equalTo(Branch(Branch(Leaf(2), Leaf(6)), Branch(Branch(Branch(Leaf(2),Leaf(3)),Leaf(5)), Leaf(3))))

  def e329_1 = Tree.size2(testTree) must equalTo(11)

  def e329_2 = Tree.maximum2(testTree) must equalTo(5)

  def e329_3 = Tree.depth2(testTree) must equalTo(4)

  def e329_4 = Tree.map2(testTree)(_ + 1) must equalTo(Branch(Branch(Leaf(2), Leaf(6)), Branch(Branch(Branch(Leaf(2),Leaf(3)),Leaf(5)), Leaf(3))))

}
