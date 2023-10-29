sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](head : A, tail : List[A]) extends List[A]

object List {
    def setHead[A](a : A, as : List[A]) : List[A] = as match {
        case Nil => Cons(a, Nil)
        case Cons(h, t) => Cons(a, t)
    }

    def tail[A](as : List[A]) : List[A] = as match {
        case Nil => Nil
        case Cons(_, t) => t
    }

    def drop[A](as : List[A], n : Int) : List[A] = (as, n) match {
        case (lst, 0) => lst
        case (Nil, _) => Nil
        case (Cons(_, t), x) => drop(t, x-1)
    }

    def apply[A](as : A*) : List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail : _*))
}

def main(args : Array[String]) : Unit = 
    println(List.drop(List(1,2,3), 1))