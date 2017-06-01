def sum(a: IndexedSeq[Int]): Int = {
  if(a.size <= 1){
    a.headOption.getOrElse(0)
  }
  else{
   val (l,r) = a.splitAt(a.size/2) 
   sum(l) + sum(r)
  }
}

println(sum(IndexedSeq(2,3,4)))



def unit[A](a: => A): Par[A]
