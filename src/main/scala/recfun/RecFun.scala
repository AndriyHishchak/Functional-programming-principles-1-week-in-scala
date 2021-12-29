package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * This method keep an index of rows and columns and check if a point is the first or last value in the row
   * if first or last, point = 1. Else else take position and sum the previous row n & n-1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || c == r) 1 else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = ???

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
