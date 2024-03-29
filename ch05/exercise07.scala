case object Empty extends Stream[Nothing]
case class Cons[A](hd : () => A, tl : () => Stream[A]) extends Stream[A]

def cons[A](hd : => A, tl : => Stream[A]) : Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
}

def empty[A] : Stream[A] = Empty

def apply[A](as : A*) : Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail : _*))

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
            case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
            case Cons(h,t) if n == 1 => cons(h(), empty)
            case _ => empty
        }
            
    def drop(n : Int) : Stream[A] =
        this match {
            case Cons(h,t) if n > 1 => t().drop(n-1)
            case Cons(h,t) if n == 1 => t()
            case _ => Empty
        }

    def takeWhile(p : A => Boolean) : Stream[A] =
        foldRight(empty)((a,b) => if (p(a)) cons(a,b) else empty)

    def foldRight[B](z : => B)(f : (A, => B) => B) : B =
        this match {
            case Cons(h,t) => f(h(), t().foldRight(z)(f))
            case _ => z
        }

    def forall(p : A => Boolean) : Boolean =
        foldRight(true)((a,b) => p(a) && b)

    def map[B](f : A => B) : Stream[B] =
        foldRight(empty)((a,b) => cons(f(a), b))

    def filter(f : A => Boolean) : Stream[A] =
        foldRight(empty)((a,b) => if (f(a)) cons(a,b) else b)
    
    def append[B >: A](s : Stream[B]) : Stream[B] =
        foldRight(s)((a,b) => cons(a,b))

    def flatMap[B >: A](f : B => Stream[B]) : Stream[B] =
        foldRight(empty)((a,b) => f(a).append(b))
}

def main(args : Array[String]) : Unit =
    val s = Stream(2,2,4,1,3)
    val t = Stream(1,23,3,1,3)
    println(s.take(3).drop(2).toList)
    println(s.takeWhile(x => x % 2 == 0).toList)
    println(s.forall(x => x % 2 == 0))
    println(s.map(x => x + 1).toList)
    println(s.filter(x => x % 2 != 0).toList)
    println(s.append(t).toList)
    println(s.flatMap(x => Stream(x+1, x+2, x+3)).toList)