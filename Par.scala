def sum(a: IndexedSeq[Int]): Par[Int] = {
  if(a.size <= 1){
    Par.unit( a.headOption.getOrElse(0))
  }
  else{
   val (l,r) = a.splitAt(a.size/2) 
   Par.map2(Par.fork(sum(l)) ,Par.fork(sum(r))) (_ + _)
  }
}


trait Par[+A]

class PlainPar[+A](value: A) extends Par[A]{
  def get: A = value
}
class ForkPar[+A](value: => Par[A]) extends Par[A]{
  // def get: A = value
}

class MapPar[+A]( par1: Par[A], par2: Par[A], combine: (A,A) => A) extends Par[A]

object Par{
  def unit[A](a: A): Par[A] = new PlainPar(a) 
  def get[A](par: Par[A]): A = ??? 
  def map2[A]( a: Par[A], b: Par[A])(f: (A,A) => A): Par[A] = new MapPar(a,b,f)
  def fork[A]( a: => Par[A]): Par[A] = new ForkPar(a)
  def lazyUnit[A]( a: => A): Par[A] = fork(unit(a))
}

Par.unit(())
