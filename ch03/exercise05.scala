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

    def dropWhile[A](as : List[A], f : A => Boolean) : List[A] = as match {
        case Cons(a, as) => if (f(a)) dropWhile(as, f) else Cons(a, as)
        case Nil => Nil
    }

    def apply[A](as : A*) : List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail : _*))
}

def main(args : Array[String]) : Unit = 
    println(List.dropWhile(List(2,2,4,0,1,3,4,5,6), x => x % 2 == 0))