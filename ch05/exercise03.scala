case object Empty extends Stream[Nothing]
case class Cons[A](hd : () => A, tl : () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd : => A, tl : => Stream[A]) : Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A] : Stream[A] = Empty

    def apply[A](as : A*) : Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail : _*))
}

trait Stream[+A] {
    def toList : List[A] =
        @annotation.tailrec
        def helper(s : Stream[A], acc : List[A]) : List[A] =
            s match {
                case Empty => acc.reverse
                case Cons(hd,tl) => (helper(tl(), hd()::acc))
            }
        helper(this, Nil)

    def take(n : Int) : Stream[A] =
        this match {
            case Cons(h,t) if n > 1 => Cons(h, () => t().take(n-1))
            case Cons(h,t) if n == 1 => Cons(h, () => Empty)
            case _ => Empty
        }
            
    def drop(n : Int) : Stream[A] =
        this match {
            case Cons(h,t) if n > 1 => t().drop(n-1)
            case Cons(h,t) if n == 1 => t()
            case _ => Empty
        }

    def takeWhile(p : A => Boolean) : Stream[A] =
        this match {
            case Cons(h,t) if (p(h())) => Cons(h, () => t().takeWhile(p))
            case _ => Empty
        }
}

def main(args : Array[String]) : Unit =
    val s = Stream(2,2,3,4,5)
    println(s.take(3).drop(2).toList)
    println(s.takeWhile(x => x % 2 == 0).toList)