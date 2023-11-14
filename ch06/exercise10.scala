type State[S,+A] = S => (A,S)
def unit[S,A](a : A) : State[S,A] =
    s => (a, s)

def flatMap[S,A,B](f : State[S,A])(g : A => State[S,B]) : State[S,B] =
    s => {
        val (a, ns) = f(s)
        g(a)(ns)
    }

def map[S,A,B](f : State[S,A])(g : A => B) : State[S,B] =
    flatMap(f)(a => unit(g(a)))

def map2[S,A,B,C](sa : State[S,A], sb : State[S,B])(f : (A,B) => C) : State[S,C] =
    flatMap(sa)(a => map(sb)(b => f(a,b)))

def main(args : Array[String]) : Unit =
    println(map(unit(1))(x => x + 5)(unit(3)))