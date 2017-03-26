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

println(maximum(Branch(Leaf(1),Branch(Leaf(2), Leaf(3)))))
