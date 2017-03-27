sealed trait Either[+E, +A]{
  def map[B](f: A => B): Either[E, B] = this match{
    case Left(e) => Left(e)
    case Right(v) = Right(f(v))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match{
    case Left(e) => Left(e)
    case Right(v) = f(v)
  }
  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
     case Left(_) => b
     case Right(a) => Right(a)
   }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Left(e) => Left(e)
    case Right(v) =>Right(f(a,b))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing] 
case class Right[+A](value: A) extends Either[Nothing, A]
