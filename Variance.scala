trait Animal 
case class Cat() extends Animal {
  def purr = Dog() 
}
case class Dog() extends Animal

type f= Cat => Animal 
type f1= Animal => Dog 


def meow(a: Cat => Animal) = println(a(Cat()))
 
meow( _.purr)
