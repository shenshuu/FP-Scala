trait RNG {
    def nextInt : (Int, RNG)
}

type Rand[+A] = RNG => (A, RNG)

def unit[A](a : A) : Rand[A] =
    rng => (a, rng)

case class simpleRNG(seed : Long) extends RNG {
    def nextInt : (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0XBL) & 0XFFFFFFFFFFFFL
        val nextRNG = simpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }
}

def flatMap[A,B](f : Rand[A])(g : A => Rand[B]) : Rand[B] =
    rng => {
        val (n, r) = f(rng)
        g(n)(r)
    }

def map[A,B](s : Rand[A])(f : A => B) : Rand[B] =
    flatMap(s)(a => unit(f(a)))

def map2[A,B,C](ra : Rand[A], rb : Rand[B])(f : (A,B) => C) : Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

def both[A,B](ra : Rand[A], rb : Rand[B]) : Rand[(A,B)] =
    map2(ra,rb)((_,_))

def nonNegativeInt(rng : RNG) : (Int, RNG) =
    val (n, r) = rng.nextInt
    (if (n < 0) -(n + 1) else n, r)

val double : Rand[Double] =
    map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1.0))

val intDouble : Rand[(Int,Double)] =
    both(nonNegativeInt, double)

val doubleInt : Rand[(Double,Int)] =
    both(double, nonNegativeInt)

def double3(rng : RNG) : ((Double, Double, Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)

def ints(count : Int)(rng : RNG): (List[Int], RNG) =
    if count <= 0 then
        (Nil, rng)
    else 
        val (n, r1) = rng.nextInt
        val (lst, r2) = ints(count-1)(r1)
        (n::lst, r2)

def main(args : Array[String]) : Unit =
    val rng = simpleRNG(42);
    println(ints(6)(rng))
    println(double(rng))