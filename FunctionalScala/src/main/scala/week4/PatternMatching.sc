trait Expr

case class Number(n: Int) extends  Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

// Pattern Matching is a generalization of switch from Java to class hierarchies. It is expressed in scala using match.
// evaluate the expression
def eval (e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1,e2) => eval(e1) + eval(e2)
}

//show the expression
def show(e: Expr): Unit = e match {
  case Number(n) => n.toString
  case Sum(e1,e2) => show(e1) +" + " + show(e2)
}

// Insertion Sort on List
def isort(xs: List[Int]) : List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y,isort(ys))
}

// inserting element in sorted list
def insert(x: Int, xs: List[Int]) : List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if(x <= y) x :: xs else y :: insert(x,ys)
}

//concatinating two lists
def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs,ys)
}

//reversing a list
def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

// remove nth element in a list
def removeAt[T](xs: List[T], n: Int) = (xs take n) ::: (xs drop n)
