package Parallelism



object Par {


  /**
   * Divide and conquer sum
   * @param ints list to be added
   * @return
   */
  def sum(ints: IndexedSeq[Int]): Int =
    if(ints.size <= 1) ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.size/2)
      sum(l) + sum(r)
    }

}
