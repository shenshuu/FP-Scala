sealed trait Par[+A]
object Par {
    def map2[A,B,C](op1 : Par[A], op2 : Par[B])(f : (A,B) => C) : Par[C]
}