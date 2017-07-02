
trait Monoid[A] {
  def op(a1: A, a2: A) : A // op(op(a1, a2), a3) == op(a1, op(a2,a3))
  def zero: A
}


object Monoids {

  val intAddition: Monoid[Int] = monoid( _ +_ , 0) 
  val intMultiplication: Monoid[Int] = monoid( _ *_ , 1) 
  val booleanOr: Monoid[Boolean] = monoid( _ || _, false)
  val booleanAnd: Monoid[Boolean] = monoid( _ && _, true)
  def optionMonoid[A]: Monoid[Option[A]] = monoid(_ orElse _ , None)
  def endoMonoid[A]: Monoid[A => A]= monoid(_ andThen _ , {x: A => x})

  def monoid[A](f: (A, A) => A, unit: A)  =  new Monoid[A]{
    def op(a1: A, a2: A) : A = f (a1,a2)
    def zero = unit 
  }


  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =  as.map(f).foldLeft(m.zero)(m.op)
  def foldLeft[A,B](as: List[A], i: B)(f: (B,A) => B): B =  {
    val aTob = {a: A => {b: B => f(b,a)}  }
    foldMap(as,endoMonoid[B] ) (aTob) (i)

  
  }
}

