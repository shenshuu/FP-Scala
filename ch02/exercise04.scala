def uncurry[A,B,C](f: A => B => C) : (A,B) => C =
    (a,b) => f(a)(b)

def main(args : Array[String]) : Unit = 
    println(uncurry((x : Int) => (y : Int) => (x + y))(3,4))