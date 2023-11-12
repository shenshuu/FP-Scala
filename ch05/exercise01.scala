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
}

def main(args : Array[String]) : Unit =
    val s = Stream(1,2,3)
    println(s.toList)