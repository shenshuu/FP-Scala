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

def map2[A,B,C](a : Option[A], b : Option[B])(f : (A,B) => C) : Option[C] =
    (a,b) match {
        case (None,_) => None;
        case (_,None) => None;
        case (Some(a),Some(b)) => Some(f(a,b));
    }
    // a.flatMap(aa => aa.map(bb => map2((aa,bb) => f(aa,bb))))

def Try[A](a : A) : Option[A] =
    try Some(a)
    catch {case e: Exception => None}

def parseInsuranceRateQuote(
    age : String,
    numberOfSpeedingTickets : String) : Option[Double] = {
        val optAge = Try {age.toInt}
        val optTickets = Try {numberOfSpeedingTickets.toInt}
        map2(optAge, optTickets)(parseInsuranceRateQuote)
    }

def main(args : Array[String]) : Unit =
    val x = parseInsuranceRateQuote("25", "10");
    println(x);