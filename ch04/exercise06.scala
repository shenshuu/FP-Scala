case class Left[+E](value : E) extends Either[E,Nothing]
case class Right[+A](value : A) extends Either[Nothing,A]
trait Either[+E, +A] {
    def map[B](f : A => B) : Either[E,B] =
        this match {
            case Left(e) => Left(e)
            case Right(a) => Right(f(a))
        }

    def flatMap[EE >: E, B](f : A => Either[EE,B]) : Either[EE,B] =
        this match {
            case Left(e) => Left(e)
            case Right(a) => f(a)
        }

    def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
        this match {
            case Left(_) => b
            case Right(a) => Right(a)
        }

    def map2[EE >: E, B, C](b : Either[EE,B])(f : (A,B) => C) : Either[EE,C] =
        this.flatMap(aa => b.map(bb => f(aa,bb)))
}

def main(args : Array[String]) : Unit =
    val either = Right("foo");
    println(either.map(x => x.length))
    println(either.flatMap(x => Right(x.length)))