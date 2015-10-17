package FunctionalDataStructures

/**
 * Tree ADT (Algebraic Data Type
 * @tparam A type of the values within the tree
 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /**
   * Exercise 3.25
   * Write a function size that counts the number of nodes (leaves and branches) in a tree.
   */
  def size[A](ts: Tree[A]):Int =
    ts match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  /**
   * Exercise 3.26
   * Write a function maximum that returns the maximum element in a Tree[Int].
   * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)
   */
  def maximum(ts: Tree[Int]):Int =
    ts match {
      case Branch(l, r) => maximum(l) max maximum(r)
      case Leaf(v) => v
    }

  /**
   * Exercise 3.27
   * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
   */
  def depth[A](ts: Tree[A]):Int =
    ts match {
      case Leaf(_)      => 0
      case Branch(l, r) => 1 + ( depth(l) max depth(r) )
    }


  /**
   * Exercise 3.28
   * Write a function map, analogous to the method of the same name on List,
   * that modifies each element in a tree with a given function.
   */
  def map[A,B](ts: Tree[A])(f: A => B): Tree[B] =
    ts match {
      case Leaf(v)    => Leaf(f(v))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }


  /**
   * Exercise 3.29
   * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
   * Reimplement them in terms of this more general function. Can you draw an analogy between this fold function and
   * the left and right folds for List?
   */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B =
    t match {
      case Leaf(v)      => f(v)
      case Branch(l,r)  => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def size2[A](ts: Tree[A]):Int = fold(ts)(_ => 1)( 1 + _ + _ )

  def maximum2(ts: Tree[Int]):Int = fold(ts)( a => a )( (a:Int,b:Int) => a max b)

  def depth2[A](ts: Tree[A]):Int = fold(ts)( _ => 0 )( (a,b) => 1 + ( a max b ))

  def map2[A,B](ts: Tree[A])(f: A => B): Tree[B] = fold(ts)( a => Leaf(f(a)):Tree[B] )( Branch(_,_) )

}