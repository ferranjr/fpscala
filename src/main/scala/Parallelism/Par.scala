package Parallelism

import java.util.concurrent._
import language.implicitConversions


object Par {


  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }


  /**
   * Data Type to represent our Parallelism
   */
  type Par[A] = ExecutorService => Future[A]

  object Par {

    /**
     * Creates a computation than immediatelly results in the value a.
     */
    def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)


    /**
     * Marks a computation for concurrent evaluation by run.
     */
    def fork[A](a: => Par[A]): Par[A] =
      (es: ExecutorService) => {
        es.submit( new Callable[A]{
          override def call(): A = a(es).get
        })
      }


    /**
     * Wraps the value a for a concurrent evaluation by run.
     */
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    /**
     * Write a function to convert any function A => B to one that evaluates
     * its result asynchronously
     */
    def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))


    /**
     * Order a List[Int] in a Parallel computation
     */
    def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
      parList.map( _.sorted )


    /**
     * to combine N parallel computations
     */
    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
      val res: List[Par[B]] = ps.map( asyncF(f) ) // We are missing sequence
      sequence(res)
    }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) as.head.map(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        sequenceBalanced(l).map2( sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      sequenceBalanced(as.toIndexedSeq).map(_.toList)
  }


  implicit class ParOps[A](in: Par[A]) extends AnyVal {

    /**
     * Combines the results of two parallel computations
     * with a binary function.
     */
    def map2[B,C](b: Par[B])(f: (A, B) => C ): Par[C] =
      (es: ExecutorService) => {
        val aa = in(es)
        val bb = b(es)

        UnitFuture( f(aa.get, bb.get ))
      }

    /**
     * Apply a function into a Par value
     */
    def map[B](f: A => B): Par[B] =
      in.map2(Par.unit(()))((a, _) => f(a))

    /**
     * Fully evaluates a given Par, spawning concurrent
     * parallel computations as requested by fork and extracting
     * the resulting value.
     */
    def run(es: ExecutorService): Future[A] = in(es)
  }


  /**
   * Divide and conquer sum
   * @param ints list to be added
   * @return
   */
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if(ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.size/2)
      sum(l).map2( sum(r) )( _ + _ )
    }

//  val test = sum(IndexedSeq(1,2,3,4,5,6,7,8,9,10,11,12)).run()
}
