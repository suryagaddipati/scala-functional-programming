type State[M,+A] = M => (M,A)

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)
type MachineState =  State[Machine,(Int,Int)]

def simulateMachine(inputs: List[Input]): MachineState = inputs match {

  case x :: xs  =>{ m: Machine =>
     val machine = x match {
      case Coin =>  Machine(!m.locked, m.candies, m.coins + 1)
      case Turn =>  Machine(!m.locked, m.candies-1, m.coins )
    }
     simulateMachine(xs)(machine)
  }

  case _ =>  m: Machine => (m, (m.candies, m.coins))
}

val machine = Machine(true,10,5)
println (simulateMachine(List(Coin,Turn, Coin,Turn)) (machine))



