case class Some[+A](get : A) extends Option[A]
case object None extends Option[Nothing]

trait Option[+A] {
    def map[B](f: A => B): Option[B] =
        this match {
            case None => None;
            case Some(a) => Some(f(a));
        }
    
    def flatMap[B](f: A => Option[B]): Option[B] =
        this match {
            case None => None;
            case Some(a) => f(a);
        }

    def getOrElse[B >: A](default: => B): B =
        this match {
            case None => default;
            case Some(a) => a;
        }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
        this match {
            case None => ob;
            case Some(a) => Some(a);
        }

    def filter(f: A => Boolean): Option[A] =
        this match {
            case None => None;
            case Some(a) => if (f(a)) Some(a) else None;
        }
}

def main(args : Array[String]) : Unit =
    val int_opt = Some(5);
    val null_opt = None;
    println(int_opt.map(x => x + 1));
    println(int_opt.flatMap(x => Some(0)));
    println(int_opt.filter(x => x % 2 == 0));
    println(null_opt.getOrElse(10));
    println(null_opt.orElse(Some(12)));