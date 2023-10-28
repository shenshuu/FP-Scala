def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a,b)

def main(args : Array[String]) : Unit = 
    println(curry((x : Int, y : Int) => x + y)(1)(2))