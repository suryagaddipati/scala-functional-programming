object Sorted{
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(idx :Int): Boolean = if(idx == as.size -1) true else if(ordered(as(idx+1), as(idx))) loop(idx+1) else false

    loop(0)
  }

  def main(args: Array[String]): Unit = println(isSorted(Array(1,2,3,4,3), (x: Int, y: Int) => x > y))
}
