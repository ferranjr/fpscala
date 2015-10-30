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

/**
 * Defining a more generic State
 *
 * type State[S,+A] = S => (A,S)
 *
 * but we can create its own class so we can add the methods we created for Rand that will be of a good use
 *
 * Exercise 6.10
 * Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods on the State case class
 * where possible. Otherwise you should put them in a State companion object.
 */
case class State[S,+A](run: S => (A, S)){

  import State._

  def map[B](f: A => B):State[S,B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap{ a =>
      sb.map( b => f(a,b) )
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

object State {

  def unit[S,A](a: A): State[S,A] =
    State( s => (a, s) )

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_::_))


  /**
   * Modify State directly on a functional way
   */

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}


/**
 * Exercise 6.11
 * Hard: To gain experience with the use of State, implement a finite state automaton that models a simple candy
 * dispenser.
 * The machine has two types of input:
 *   1. you can insert a coin, or
 *   2. you can turn the knob to dispense candy.
 * It can be in one of two states:
 *   1. locked or
 *   2. unlocked. It also tracks how many candies are left and how many coins it contains.
 *
 *   The rules of the machine are as follows:
 *   . Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
 *   . Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
 *   . Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
 *   . A machine that’s out of candy ignores all inputs.
 *
 *   The method simulateMachine should operate the machine based on the list of inputs and return the number of coins
 *   and candies left in the machine at the end. For example, if the input Machine has 10 coins and 5 candies, and a
 *   total of 4 candies are successfully bought, the output should be (14, 1).
 */

sealed trait Input
case object Coin extends Input
case object Knob extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  import State._

  def update = (i: Input) => (s: Machine ) =>
    (i,s) match {
      case (_, Machine(_, 0,_))                   => s
      case (Knob, Machine(true, _,_))             => s
      case (Coin, Machine(false, _,_))            => s
      case (Coin, Machine(true, candies, coins))  => Machine(locked = false, candies, coins + 1)
      case (Knob, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for{
      _     <- sequence(inputs map (modify[Machine] _ compose update))
      s     <- get
    } yield (s.coins, s.candies)

}


