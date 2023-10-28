def fibonacci(n: Int) : Int =
    @annotation.tailrec
    def helper(fib1: Int, fib2: Int, n: Int) : Int =
        if (n == 0) fib1
        else helper(fib2, fib1 + fib2, n - 1)
    helper(0, 1, n)

def main(args : Array[String]) : Unit =
    println(fibonacci(0) == 0)
    println(fibonacci(1) == 1)
    println(fibonacci(3) == 2)
    println(fibonacci(6) == 8)