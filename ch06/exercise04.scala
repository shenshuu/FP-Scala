trait RNG {
    def nextInt : (Int, RNG)
}

type Rand[+A] = RNG => (A, RNG)

case class simpleRNG(seed : Long) extends RNG {
    def nextInt : (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0XBL) & 0XFFFFFFFFFFFFL
        val nextRNG = simpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }
}

def map[A,B](s : Rand[A])(f : A => B) : Rand[B] =
    rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
    }

def nonNegativeInt(rng : RNG) : (Int, RNG) =
    val (n, r) = rng.nextInt
    (if (n < 0) -(n + 1) else n, r)

def double(rng : RNG) : (Double, RNG) =
    map(nonNegativeInt)(n => n / Int.MaxValue.toDouble)

def intDouble(rng : RNG) : ((Int, Double), RNG) =
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i,d), r2)

def doubleInt(rng : RNG) : ((Double, Int), RNG) =
    val ((i,d), r) = intDouble(rng)
    ((d,i), r)

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