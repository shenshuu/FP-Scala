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

def main(args : Array[String]) : Unit =
    val rng = simpleRNG(42);
    println(double(double(rng)._2))