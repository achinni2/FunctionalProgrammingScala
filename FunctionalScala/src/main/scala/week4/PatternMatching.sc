def times(chars: List[Char]): List[(Char, Int)] = {
  var result = chars.map(c => (c,1))
    .groupBy(_._1)
    .mapValues(_.map(_._2).foldLeft(0)(_ + _)).toList

  result.sortBy(_._2)
}

times(List('a','b','c','a'))