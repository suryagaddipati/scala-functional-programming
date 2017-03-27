sealed trait Either[+E, +A]{
  def map[B](f: A => B): Either[E, B] = this match{
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match{
    case Left(e) => Left(e)
    case Right(v) => f(v)
  }
  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
     case Left(_) => b
     case Right(a) => Right(a)
   }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =for { a <- this; b1 <- b } yield f(a,b1) 
}
case class Left[+E](value: E) extends Either[E, Nothing] 
case class Right[+A](value: A) extends Either[Nothing, A]

//should return the first error that’s encountered, if there is one.
def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
  case  Nil => Right(Nil)
  case h :: t => h match {
    case Left(e) => Left(e)
    case Right(hv) => sequence(t) match {
      case Left(err) => Left(err)
      case Right(tailList) => Right(hv :: tailList)
    }
  }
}
def sequence_1[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
  case  Nil => Right(Nil)
  case h :: t => h.flatMap(hval => sequence(t).map(tList => hval:: tList))
}
// println(sequence_1(List(Right("meow"),Right("purr"))))
// println(sequence_1(List(Right("meow"),Left("woof"))))


//should return the first error that’s encountered, if there is one.
def traverse[E, A, B](as: List[A])( f: A => Either[E, B]): Either[E, List[B]]= as match {
  case  Nil => Right(Nil)
  case h :: t => f(h).flatMap( hh =>  traverse(t)(f).map(tList => hh :: tList))
}

println(traverse(List(1,2,3))(num => Right(num) ))
println(traverse(List(1,2,3))(num => if(num%2 ==0)Left("meow") else Right(num+2)))
