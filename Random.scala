

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}
//copy pasta from book
case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }
}

// println(Simple(1).nextInt._1)

def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match{
  case (i , nRng) if( i < 0) => (-(i + 1), nRng)
  case (i,nRng) => (i,nRng)
}
// println(nonNegativeInt(Simple(1))._1)

def ints(count: Int)(rng: RNG): (List[Int], RNG)  = count match {
   case 0 => (Nil,rng)
   case _ => {
     val (l,nRng) =rng.nextInt 
     val (nextL, nnRng) = ints(count-1)(nRng)
     ((l :: nextL),nnRng)
   }
}


// println(ints(5)(Simple(1))._1)

type Rand[A] = RNG => (A,RNG)

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =  {
  seedRng: RNG => {
     val (a,rNext) = ra(seedRng)
     val (b,rNext1) = rb(rNext)
     (f(a,b), rNext1)
  }
}

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =  fs match {
  case Nil => (rng => (Nil,rng))
  case h :: t => (rng => {
    val (headValue,nRng) = h(rng)
    val (tailListValues, nnRng)=sequence(t)(nRng)
    (headValue::tailListValues, nnRng)
  })
}


def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG)  = sequence( List.fill(count)(((r: RNG) => r.nextInt)) )(rng)
println(intsViaSequence(5)(Simple(1))._1)

