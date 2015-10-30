package PurelyFunctionalState


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }

  }

  /**
   * Exercise 6.1
   * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
   * Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart.
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (res, seed) = rng.nextInt
    (if(res < 0) -(res + 1) else res, seed)
  }

  /**
   * Exercise 6.2
   * Write a function to generate a Double between 0 and 1, not including 1. Note: You can use Int.MaxValue to obtain
   * the maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double.
   */
  def double(rng: RNG): (Double, RNG) = {
    val (res, seed) = nonNegativeInt(rng)
    (res/(Int.MaxValue.toDouble + 1), seed)
  }


  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  /**
   * Exercise 6.3
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
   * You should be able to reuse the functions you’ve already written.
   */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i,d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = r.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r)   = double(rng)
    val (d2, r2)  = double(r)
    val (d3, r3)  = double(r2)
    ((d1,d2,d3), r3)
  }

  /**
   * Exercise 6.4
   * Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if(count > 0){
        val (i, r) = rng.nextInt
        val (l, r2) = ints(count-1)(r)
        (i :: l, r2)
    }
    else {
      (List(), rng)
    }


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)


  /**
   * Exercise 6.5
   * Use map to reimplement double in a more elegant way. See exercise 6.2.
   */
  def double2: Rand[Double] =
    map(nonNegativeInt)(_/(Int.MaxValue.toDouble + 1))


  /**
   * Exercise 6.6
   * Write the implementation of map2 based on the following signature. This function takes two actions, ra and rb,
   * and a function f for combining their results, and returns a new action that combines them:
   */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  /**
   * Exercise 6.7
   * Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
   * Implement sequence for combining a List of transitions into a single transition. Use it to reimplement
   * the ints function you wrote before. For the latter, you can use the standard library function List.fill(n)(x)
   * to make a list with x repeated n times.
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f,acc)(_::_))

  def intsWithSequence(n: Int): Rand[List[Int]] =
    sequence(List.fill(n)(int))

  /**
   * Exercise 6.8
   * Implement flatMap, and then use it to implement nonNegativeLessThan.
   */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng =>{
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int):Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  /**
   * Exercise 6.9
   * Reimplement map and map2 in terms of flatMap. The fact that this is possible is what we’re referring to when we
   * say that flatMap is more powerful than map and map2.
   */
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(i => unit(f(i)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ i =>
      map(rb){ j => f(i,j) }
    }


  /**
   * More testable rolldie
   */
  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

