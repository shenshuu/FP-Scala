def isSorted[A](as : Array[A], ordered : (A,A) => Boolean) : Boolean =
    def helper(i : Int) : Boolean = 
        if (i == as.length - 1) true
        else ordered(as(i), as(i+1)) && helper(i+1)
    helper(0)

def main(args : Array[String]) : Unit = 
    println(isSorted(Array(3,5,6), (x, y) => x <= y))
    println(isSorted(Array(3,7,6), (x, y) => x <= y))