import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =this match{
    case Some(x) => Some(f(x))
    case None => None 
  }
  def flatMap[B](f: A => Option[B]): Option[B] =  this match {
    case Some(x) => f(x)
    case None => None 
  }
  //indicates that the argument is of type B, but won’t be evaluated until it’s needed by the function. 
  //if the Option is None, returns the given default value.
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
   //returns the first Option if it’s defined; otherwise, it returns the second Option.
  def orElse[B >: A](ob: => Option[B]): Option[B] =   this.map(Some(_)).getOrElse(ob)

  //Convert Some to None if the value doesn’t satisfy f.
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case None => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap( m => mean(xs.map(x => math.pow(x - m, 2))))

//if either Option value is None, then the return value is too. 
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match{
  case(None,_) => None
  case(_,None) => None
  case(Some(a),Some(b)) => Some(f(a,b))
}
