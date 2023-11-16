trait Monoid[A] {
    def op(a1 : A, a2 : A) : A
    def zero: A
}

def optionMonoid[A] : Monoid[Option[A]] = new:
    def op(a1: Option[A], a2 : Option[A]) : Option[A] =
        a1 orElse a2
    val zero = None

def main(args : Array[String]) : Unit =
    println("foo")