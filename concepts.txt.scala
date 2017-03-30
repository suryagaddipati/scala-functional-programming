1. Case classes

2. Type Alias Functions 
type Environment = String => Int

3. case expressions can have guards 
case Var(n) if (v == n) => Const(1)

4. val env: Environment = { case "x" => 5 case "y" => 7 }
env(x) == 5

5. An ADT is just a data type defined by one or more data constructors,each of which may contain zero or more arguments. We say that the data type is the sum or union of its data construc- tors, and each data constructor is the product of its arguments, hence the name alge- braic data type.14

6. we say that a non-strict function in Scala takes its argu-ments by name rather than by value. non-strict == doesn't evaluate the args
  def maybeTwice2(b: Boolean, i: => Int) <-- syntax for lazy arg

7. smart constructors  

