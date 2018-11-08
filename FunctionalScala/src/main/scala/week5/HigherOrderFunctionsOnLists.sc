//transforming each element of a list
def scaleList(xs: List[Double],factor: Double): List[Double] = xs match {
  case Nil => Nil
  case y :: ys => y*factor :: scaleList(ys,factor)
}

// transforming each element of a list using map
def scaleList1(xs: List[Double],factor: Double): List[Double] = xs map (x => x*factor)

// Filtering elements based on a criteria (positive elements)
def posElements(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => if (y > 0) y :: posElements(ys) else posElements(ys)
}

//splitting data into packs
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case a :: ab =>
    val (first,rest) = xs span (y => y == a)
    first :: pack(rest)
}

val data = List("a","a","a","b","b","c","c","a")
pack(data)

//run length encoding
def encode[T](xs: List[T]):List[(T,Int)] =
  pack(xs) map (ys => (ys.head,ys.length))

encode(data)
