def accumulate(combine_func: Int, Int => Int, start: Int)(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int) : Int = {
    if (a > b) acc
    else loop(a+1, combine_func(f(a), acc))
  }
  loop(a, start)
}
