sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head : A, tail : List[A]) extends List[A]

object List {

    def tail[A](xs : List[A]) : List[A] = xs match {
        case Nil => Nil
        case Cons(_, t) => t
    }
        
    def apply[A](as : A*) : List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail : _*))

}

def main(args : Array[String]) : Unit = 
    println(List.tail(List(1,2,3)))