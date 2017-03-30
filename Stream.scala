import Stream._
sealed trait Stream[+A]{
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h,t) => h() :: t().toList
  }

  def take(n: Int): List[A] =   {
    @annotation.tailrec
    def go(c: List[A], s: Stream[A]): List[A] = if(c.size == n) c else s match {
      case Empty => c 
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

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((a,cStream)=> cons(a,cStream))

  def map[B](f: A => B): Stream[B] =  foldRight[Stream[B]](Empty)((a,cStream)=> cons(f(a),cStream))

  def headOptionViaFoldRight : Option[A] =  foldRight[Option[A]](None)((a,option) => option match {
    case None => Some(a)
    case Some(x) => Some(a)
  })

  def startsWith[A](s: Stream[A]): Boolean = s match {
    case Empty => true
    case Cons(h,t) => this match {
      case Empty => false
      case Cons(thisH,thisT) =>  thisH() == h() && thisT().startsWith( t())
    }
  }

  def tails: Stream[Stream[A]] = unfold(this)(stream => stream match{
    case Empty => None
    case Cons(h,t) => Some(stream,t())
  })

  def scanRight[B](i: B) (p: (A,=>B) => B): Stream[B] = {
    def go(t : Stream[Stream[A]]): Stream[B] = t match {
      case Empty => Stream(i)
      case Cons( hStream, tStream) => cons( hStream().foldRight(i)(p) , go(tStream()))
    }
    go(tails)

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
      println(Stream(1,2,3).scanRight(0)(_ + _).toList)
      // println( Stream(1,2,3).tails.toList.map(_.toList))    
      // println(Stream(1,2,3).startsWith(Stream(1,2)))
      // println(Stream(1,2,3,4).startsWith(Stream(1,2,3)))
      // println(Stream(1,2,3,4).startsWith(Stream(2,3)))

      // println(Stream(1,2).toList)
      // println(Stream(1,2,3,4).take(3))
      // println(Stream(2,4,7,8).takeWhile(_%2 == 0).toList)
      // println(Stream(2,4,7,8).forAll(_ %2 ==0))
      // println(Stream(2,4,8).forAll(_ %2 ==0))
      // println(Stream(2,4,7,8).takeWhileViaFoldRight(_%2 == 0).toList)
      // println(Stream(2,4,8).headOption)
      // println(Stream(2,4,8).headOptionViaFoldRight)
      // println(Stream(2,4,8).map(_ + 2).toList)
      // println(Stream(2,4,8).append(Stream(22,343)).toList)


      def constant[A](a: A): Stream[A] = cons(a,constant(a))
      // println(constant(3).take(5).toList)

      def from(n: Int): Stream[Int] = cons(n,from(n+1))
      // println(from(3).take(5).toList)

      def fibs:Stream[Int] = {
        def go(n1: Int , n2: Int): Stream[Int] = cons(n1,go(n2,n1+n2))
        go(0,1)
      }
      // println(fibs.take(7).toList)

      def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).map( a => cons(a._1,unfold(a._2)(f)) ) match {
        case Some(x) => x
        case None => Empty 
      }
      // println( unfold(1)( a => Some(a.toString,a+10)).take(6).toList)

      def fibsViaUnfold:Stream[Int] = unfold((0,1))(a => Some(a._1, (a._2, a._1+a._2)))
      // println(fibsViaUnfold.take(7).toList)

      def constantViaUnfold[A](a: A): Stream[A] = unfold(a) (a => Some(a, a))
      // println(constantViaUnfold(3).take(5).toList)

      def zipWith[A,B,C](a: Stream[A] , b: Stream[B])(f: (A,B) => C): Stream[C] = unfold((a,b)) ( a => (a._1, a._2) match {
        case( Cons(ha,ta),Cons(hb,tb)) => Some(f(ha(),hb()), (ta(),tb()))
        case _ => None
      })

      // println(zipWith(Stream(1,2,3), Stream(1,2,3))(_+_).toList)

