package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c > r) 0 else if (c == 0) 1 else
      pascal(c-1, r-1) + pascal(c, r-1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def iterate(chars: List[Char], level: Int): Boolean =
        if (level < 0) false else
        if (chars.isEmpty) level == 0 else {
          val head = chars.head
          val delta_level = if (head == '(') 1 else if (head == ')') -1 else 0
          iterate(chars.tail, level + delta_level)
        }

      iterate(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      // The plan of attack here:
      // for every denomination in the list:
      //   for every multiple of that denomination less than the total:
      //     Recursively compute the ways to make change of the remaining
      //      sum using the other coins.

      def calc_for_head(qty: Int, coins: List[Int]): Int = {
        if (coins.isEmpty) 0 else {
          val head_denom_sum = qty * coins.head
          if (head_denom_sum < money) {
            calc_for_head(qty + 1, coins) +
            countChange(money - head_denom_sum, coins.tail)
          } else if (head_denom_sum == money) 1 else 0
        }
      }

      if (money == 0 || coins.isEmpty) 0 else {
        calc_for_head(1, coins) + countChange(money, coins.tail)
      }
    }
  }
