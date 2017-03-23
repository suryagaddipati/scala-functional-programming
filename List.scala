trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

    // val x = List(1,2,3,4,5) match {
    //   case Cons(x, Cons(2, Cons(4, _))) => x
    //   case Nil => 42
    //   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h 
    //   case _ => 101
    // }
    // println(List(1,2,3).sum())
}

def sum(as: List[Int]): Int = as match {
  case Nil => 0
  case Cons(x,xs) => x + sum(xs)
}

def tail[A](as : List[A]): List[A] = as match{
  case Nil => Nil
  case Cons(_,tail) => tail
}
def setHead[A](a: A , as: List[A]): List[A] = as match{
  case Nil => Nil
  case Cons(_,tail) => Cons(a,tail)
}

def drop[A](l: List[A], n: Int): List[A] = {
  def loop(o: List[A], c:Int): List[A] = if(c>=n)  o else loop(tail(o),c +1)
  loop(l,0)
}

def dropWhile[A](l: List[A], f: A => Boolean): List[A] =  
  l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }



println(dropWhile(List(4,5,6,7,8),(a: Int)=> a % 2 == 0))



