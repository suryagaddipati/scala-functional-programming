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
println(Stream(2,4,7,8).takeWhile(_%2 == 0).toList)

