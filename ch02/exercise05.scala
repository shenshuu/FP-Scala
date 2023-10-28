def compose[A,B,C](f : A => B, g : B => C) : A => C =
    a => g(f(a))

def main(args : Array[String]) : Unit = 
    println(compose(Math.abs, (x : Int) => x + 10)(-16))