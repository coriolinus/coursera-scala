def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x > y) y :: insert(x, ys) else x :: y :: ys
}
