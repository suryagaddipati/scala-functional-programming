import Stream._
sealed trait Stream[+A]{
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h,t) => h() :: t().toList
  }

  def take(n: Int): List[A] =   {
    @annotation.tailrec
    def go(c: List[A], s: Stream[A]): List[A] = if(c.size == n) c else s match {
      case Empty => List.empty
      case Cons(h,t) => go(c :+ h(),t())
    }
    go(List.empty,this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match{
    case Cons(h,t) if(p(h())) =>  Cons(h,() => t().takeWhile(p))
    case _ => Empty 
  }

  def forAll(p: A => Boolean): Boolean =  this match {
    case Empty => true
    case Cons(h,t) => p(h()) && t().forAll(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f)) 
    case _ => z
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight[Stream[A]](empty)((a,list) => if(p(a)) cons(a,list) else empty)

  def headOption: Option[A] = this match { 
    case Empty => None
    case Cons(h, t) => Some(h())
  }
  def headOptionViaFoldRight : Option[A] =  foldRight[Option[A]](None)((a,option) => option match {
     case None => Some(a)
     case Some(x) => Some(a)
  })
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}


// println(Stream(1,2).toList)
// println(Stream(1,2,3,4).take(3))
// println(Stream(2,4,7,8).takeWhile(_%2 == 0).toList)
// println(Stream(2,4,7,8).forAll(_ %2 ==0))
// println(Stream(2,4,8).forAll(_ %2 ==0))
// println(Stream(2,4,7,8).takeWhileViaFoldRight(_%2 == 0).toList)
 // println(Stream(2,4,8).headOption)
 println(Stream(2,4,8).headOptionViaFoldRight)


