case class Some[A](get : A) extends Option[A]
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

def Try[A](a : A) : Option[A] =
    try Some(a)
    catch {case e: Exception => None}

def traverse[A,B](as : List[A])(f : A => Option[B]) : Option[List[B]] =
    as match {
        case Nil => Some(Nil)
        case h::t => f(h).flatMap(hh => traverse(t)(f).map(hh::_))
    }

def sequence[A](as : List[Option[A]]) : Option[List[A]] =
    traverse(as)(a => a)

def main(args : Array[String]) : Unit =
    println(sequence(List(Some(5),Some(1))))
    println(traverse(List(5,1))(x => Some(x+1)))