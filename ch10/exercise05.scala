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

def foldMap[A,B](as : List[A], m : Monoid[B])(f : A => B) : B =
    as.foldLeft(m.zero)((acc, a) => m.op(f(a), acc))

def main(args : Array[String]) : Unit =
    val intAddition : Monoid[Int] = new:
        def op(x : Int, y : Int) : Int = x + y
        val zero = 0
    println(foldMap(List(1,2,3,4,5), intAddition)(identity))