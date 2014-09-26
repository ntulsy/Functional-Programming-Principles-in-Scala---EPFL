package recfun
import common._

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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  // pattern matching practice
  // note the usage of case statment and pattern matching in scala
  def balance(chars: List[Char]): Boolean = {
    def matchLoop (x: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) x == 0
      else (x, chars.head) match {
        case (0, ')') => return false // must use return to force exit
        case (x, '(') => matchLoop(x + 1, chars.tail)
        case (x, ')') => matchLoop(x - 1, chars.tail)
        case _ => matchLoop(x, chars.tail) // default
      }
    } 
  matchLoop(0, chars)
  }
  /**
   * Exercise 3
   */
  // last if: there are to ways to divide. Use this coin, or don't use it. 
  def countChange(money: Int, coins: List[Int]): Int = 
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}
