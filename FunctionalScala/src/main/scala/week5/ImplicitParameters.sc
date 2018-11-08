import math.Ordering
// sorting the list with any type using merge sort.
def msort[T](xs: List[T])(lt:(T,T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs,ys) match {
        case (Nil,ys) => ys
        case (xs,Nil) => xs
        case (x :: xs1,y :: ys1) =>
          if (lt(x,y)) x :: merge(xs1,ys)
          else y :: merge(xs,ys1)

      }
    val (fst,snd) = xs splitAt n
    merge(msort(fst)(lt),msort(snd)(lt))
  }
}

// sorting using scala ordering functions.
def msort1[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs,ys) match {
        case (Nil,ys) => ys
        case (xs,Nil) => xs
        case (x :: xs1,y :: ys1) =>
          if (ord.lt(x,y)) x :: merge(xs1,ys)
          else y :: merge(xs,ys1)

      }
    val (fst,snd) = xs splitAt n
    merge(msort1(fst),msort1(snd))
  }
}

//testing
val nums = List(2,-4,3,1,7,6)
msort(nums)((x,y) => x < y)
msort1(nums)
