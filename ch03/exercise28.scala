sealed trait Tree[+A]
case class Leaf[A](value : A) extends Tree[A]
case class Branch[A](left : Tree[A], right : Tree[A]) extends Tree[A]

object Tree {
    def size[A](tree : Tree[A]) : Int =
        tree match {
            case Leaf(_) => 1
            case Branch(l,r) => 1 + size(l) + size(r)
        }

    def maximum(tree : Tree[Int]) : Int =
        tree match {
            case Leaf(x) => x
            case Branch(l,r) => maximum(l).max(maximum(r))
        }

    def depth[A](tree : Tree[A]) : Int =
        tree match {
            case Leaf(_) => 0
            case Branch(l,r) => 1 + depth(l).max(depth(r))
        }

    def map[A,B](tree : Tree[A], f : A => B) : Tree[B] =
        tree match {
            case Leaf(v) => Leaf(f(v))
            case Branch(l,r) => Branch(map(l,f), map(r,f))
        }
}

def main(args : Array[String]) : Unit =
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    println(Tree.map(tree, x => x+1))