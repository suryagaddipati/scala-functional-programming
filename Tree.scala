sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l,r) => size(l) + size(r)
}
// println(size(Branch(Leaf(1), Leaf(1))))


def maximum(t: Tree[Int]): Int = t match {
  case Leaf(v) => v
  case Branch(l,r) => maximum(l).max(maximum(r)) 
}
// println(maximum(Branch(Leaf(1),Branch(Leaf(2), Leaf(3)))))

def depth[A](t: Tree[A]): Int = t match {
  case Leaf(v) => 0
  case Branch(l,r) => (depth(l)+1).max(depth(r)+1) 
}
// println(depth(Branch(Branch(Leaf(2), Leaf(3)),Branch(Branch(Leaf(2),Leaf(4)), Leaf(3)))))
// println(depth(Branch(Leaf(2),Branch(Leaf(2), Leaf(3)))))

def map[A,B](t: Tree[A])(f: A => B) : Tree[B] = t match {
  case Leaf(v) => Leaf(f(v))
  case Branch(l,r) => Branch(map(l)(f),map(r)(f)) 
}
// println(map(Branch(Leaf(1), Leaf(2)))(_+1))

def fold[A,B](t:Tree[A])(f1: A => B)(f2: (B,B) => B): B =  t match {
  case Leaf(v) => f1(v)
  case Branch(l,r) => f2(fold(l)(f1)(f2),  fold(r)(f1)(f2))
}


def maximumViaFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)
println(maximumViaFold(Branch(Leaf(1),Branch(Leaf(2), Leaf(3)))))
