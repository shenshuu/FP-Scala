trait Monoid[A] {
    def op(a1 : A, a2 : A) : A
    def zero: A
}

def optionMonoid[A] : Monoid[Option[A]] = new:
    def op(a1: Option[A], a2 : Option[A]) : Option[A] =
        a1 orElse a2
    val zero = None

def endoMonoid[A] : Monoid[A => A] = new:
    def op(a1 : A => A, a2 : A => A) : A => A =
        a1 andThen a2
    val zero : A => A = identity

def main(args : Array[String]) : Unit =
    println("foo")