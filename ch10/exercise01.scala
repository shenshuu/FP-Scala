trait Monoid[A] {
    def op(a1 : A, a2 : A) : A
    def zero: A
}

def main(args : Array[String]) : Unit =
    val intAddition : Monoid[Int] = new:
        def op(x : Int, y : Int) : Int = x + y
        val zero = 0
    val intMultiplication : Monoid[Int] = new:
        def op(x : Int, y : Int) : Int = x * y
        val zero = 1
    val booleanOr : Monoid[Boolean] = new:
        def op(x : Boolean, y : Boolean) = x || y
        val zero = false
    val booleanAnd : Monoid[Boolean] = new:
        def op(x : Boolean, y : Boolean) = x && y
        val zero = true
    