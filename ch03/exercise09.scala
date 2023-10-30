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

    def init[A](l : List[A]) : List[A] = l match {
        case Nil => Nil
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
    }

    def foldRight[A,B](as : List[A], z : B)(f : (A,B) => B) : B =
        as match {
            case Nil => z
            case Cons(h, t) => f(h, foldRight(t,z)(f))
        }

    def length[A](as : List[A]) =
        foldRight(as, 0)((_, acc) => acc + 1)

    def apply[A](as : A*) : List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail : _*))
}

def main(args : Array[String]) : Unit = 
    println(List.length(List(1,2,3,4)))