trait RNG {
    def nextInt : (Int, RNG)
}

case class simpleRNG(seed : Long) extends RNG {
    def nextInt : (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0XBL) & 0XFFFFFFFFFFFFL
        val nextRNG = simpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }
}

def nonNegativeInt(rng : RNG) : (Int, RNG) =
    val (n, r) = rng.nextInt
    (if (n < 0) -(n + 1) else n, r)

def double(rng : RNG) : (Double, RNG) =
    val (n, r) = nonNegativeInt(rng)
    (n / (Int.MaxValue + 1.0), r)

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

def main(args : Array[String]) : Unit =
    val rng = simpleRNG(42);
    println(intDouble(rng))
    println(doubleInt(rng))
    println(double3(rng))