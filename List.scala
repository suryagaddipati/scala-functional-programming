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
  case Cons(x,Nil) => List(x)
  case Cons(_,tail) => tail
}
def setHead[A](a: A , as: List[A]): List[A] = as match{
  case Nil => Nil
  case Cons(x,Nil) => List(a)
  case Cons(_,tail) => Cons(a,tail)
}

println(setHead(2, List(4,5)))



