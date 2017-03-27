sealed trait Either[+E, +A]{
  def map[B](f: A => B): Either[E, B] = this match{
    case Left(e) => Left(e)
    case Right(v) = Right(f(v))
  }
}
case class Left[+E](value: E) extends Either[E, Nothing] 
case class Right[+A](value: A) extends Either[Nothing, A]
