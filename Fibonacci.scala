object Fibonacci{
  def fib(n : Int): Int = {
    def go(n1: Int , n2: Int, counter: Int): Int = if(counter == n ) n2 else go(n2, n1+n2, counter+1)
    
    go(0,1,2)
  }
  def main(args: Array[String]): Unit = println(fib(7))
}
