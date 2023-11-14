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
    (n % Int.MaxValue, r)

def main(args : Array[String]) : Unit =
    println(nonNegativeInt(simpleRNG(42))._1)