sealed trait List[+A]
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

// println(dropWhile(List(4,5,6,7,8),(a: Int)=> a % 2 == 0))

def init[A](l: List[A]): List[A] = l match{
  case Nil => Nil
  case Cons(_,Nil) => Nil
  case Cons(h,t) => Cons(h, init(t))
}
// println(init(List(4,5,6,7,8)))

def foldRight[A,B](l: List[A], i: B)(f: (A,B) => B): B = l match {
  case Nil => i
  case Cons(h,t) => f(h , foldRight(t,i)(f)) 
}
// println( foldRight(List(1,2,3), 1) ( _ * _))
// println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

def length[A](as: List[A]): Int = foldRight(as,0)((_,b) => b +1)

// println(length(List(1,2,3)))
//

def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
  @annotation.tailrec
  def loop(l: List[A], z: B): B = l match {
    case Nil => z
    case Cons(h,t) => loop(t, f(z,h))
  }
  loop(as,z)
}

// println( foldLeft(List(1,2,3), 1) ( _ * _))
def reverse[A](l: List[A]): List[A]  = foldLeft(l, Nil:List[A])((t, h) => Cons(h,t))
// println(reverse(List(1,2,3)))

def append[A](l: List[A],i : A): List[A]  = foldRight(l, List(i))((h,t) => Cons(h,t)) 

// println(append(List(1,2,3),4))

def concat[A](a: List[A],b: List[A]): List[A]  = b match {
   case Nil => a
   case Cons(h,t) => concat(append(a,h),t)
}
// println(concat(List(1,2,3),List(4,5,6)))

def addOne(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case Cons(h,t) => Cons(h+1,addOne(t))
}

// println(addOne(List(1,2,3)))

def toString(l: List[Double]): List[String] = l match {
  case Nil => Nil
  case Cons(h,t) => Cons(h.toString,toString(t))
}
// println(toString(List(1,2,3)))
//


def map[A,B](as: List[A])(f: A => B): List[B] =   as match{
  case Nil => Nil
  case Cons(h,t) => Cons(f(h),map(t)(f))
}

// println(map(List(1,2,3))(a => a +1))


def filter[A](as: List[A])(f: A => Boolean): List[A] = as match{
  case Nil => Nil
  case Cons(h,t) => if(f(h)) Cons(h,filter(t)(f)) else filter(t)(f)
}

// println(filter(List(4,5,6,7,8))(a=> a % 2 == 0))

def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
  case Nil => Nil
  case Cons(h,t) => concat(f(h),flatMap(t)(f))
}

// println(flatMap(List(1,2,3))(i => List(i,i)))
 
def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] =  flatMap(as)(i => if(f(i)) List(i) else Nil)


// println(filter(List(4,5,6,7,8))(a=> a % 2 == 0))

def add(a: List[Int] , b: List[Int]): List[Int] = a match {
  case Nil => Nil
  case Cons(ah,at) => b match {
   case Nil => Nil
   case Cons(bh,bt) =>  Cons( ah + bh,add(at,bt))
  }
}

// println(add(List(1,2,3), List(1,2,3)))

def zipWith[A,B,C](a: List[A] , b: List[B])(f: (A,B) => C): List[C] =  (a,b) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case(Cons(ha,ta), Cons(hb,tb)) => Cons(f(ha,hb),zipWith(ta,tb)(f))
  
}
// println(zipWith(List(1,2,3), List(1,2,3))(_+_))

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
  case Nil => true
  case Cons(hSup,tSup) => sub match{
    case Nil => true
    case Cons(hSub,tSub) =>  hSub== hSup && hasSubsequence(tSup,tSub)
  }
}

println(hasSubsequence(List(1,2,3,4),List(1,2)))
println(hasSubsequence(List(1,2,3,4),List(1,4)))

