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

    @annotation.tailrec
    def foldLeft[A,B](as : List[A], z : B)(f : (B,A) => B) : B =
        as match {
            case Nil => z
            case Cons(h,t) => foldLeft(t, f(z,h))(f)
        }

    def length[A](as : List[A]) =
        foldRight(as, 0)((_, acc) => acc + 1)

    def apply[A](as : A*) : List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail : _*))

    def sum(xs : List[Int]) : Int =
        foldLeft(xs, 0)(_ + _)

    def product(xs : List[Double]) : Double =
        foldLeft(xs, 1.0)(_ * _)

    def lengthAlternative[A](as : List[A]) : Int =
        foldLeft(as, 0)((acc,_) => acc + 1)

    def reverse[A](as : List[A]) : List[A] =
        foldLeft(as, Nil : List[A])((acc, a) => Cons(a,acc))

    def foldLeftAlternative[A,B](as : List[A], z : B)(f : (B,A) => B) : B =
        foldRight(reverse(as), z)((acc, a) => f(a, acc))

    def foldRightAlternative[A,B](as : List[A], z : B)(f : (A,B) => B) : B =
        foldLeft(reverse(as), z)((a, acc) => f(acc, a))

    def append[A](xs : List[A], ys : List[A]) : List[A] = 
        foldRight(xs, ys)((x, acc) => Cons(x, acc))

    def concat[A](ls : List[List[A]]) : List[A] =
        foldRight(ls, Nil : List[A])(append)

    def map[A,B](as : List[A])(f : A => B) : List[B] =
        as match {
            case Nil => Nil
            case Cons(h,t) => Cons(f(h), map(t)(f))
        }

    def filter[A](as : List[A])(f : A => Boolean) : List[A] =
        as match {
            case Nil => Nil
            case Cons(h,t) => if (f(h)) Cons(h,filter(t)(f)) else filter(t)(f)
        }

    def flatMap[A,B](as : List[A])(f : A => List[B]) : List[B] =
        concat(map(as)(f))

    def inc(xs : List[Int]) : List[Int] =
        map(xs)(x => x+1)

    def doubleToString(ds : List[Double]) : List[String] =
        map(ds)(d => d.toString())

    def add(xs : List[Int], ys : List[Int]) : List[Int] =
        (xs,ys) match {
            case (Cons(hx,tx),Cons(hy,ty)) => Cons(hx+hy, add(tx,ty))
            case _ => Nil
        }

    def zipWith[A](xs : List[A], ys : List[A], f : (A,A) => A) : List[A] =
        (xs,ys) match {
            case (Cons(hx,tx),Cons(hy,ty)) => Cons(f(hx,hy), zipWith(tx,ty,f))
            case _ => Nil
        }
}

def main(args : Array[String]) : Unit = 
    println(List.inc(List(1,2,3)))
    println(List.doubleToString(List(3.0, 4.12)))
    println(List.filter(List(1,2,3,4,5))(x => x % 2 != 0))
    println(List.flatMap(List(1,2,3))(i => List(i,i)))
    println(List.zipWith(List(1,3),List(4,5), (x,y) => x+y))