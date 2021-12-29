package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")

      println("Parentheses Balancing")
      println(" - " + balance("".toList))
      println("test - " + balance("test".toList))
      println("test( - " + balance("test(".toList))
      println("test() - " + balance("test()".toList))
      println("test()) - " + balance("test())".toList))
      println("test())( - " + balance("test())(".toList))


  /**
   * This method keep an index of rows and columns and check if a point is the first or last value in the row
   * if first or last, point = 1. Else else take position and sum the previous row n & n-1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || c == r) 1 else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * This method verifies the balancing of parentheses in a string
   */
  def balance(chars: List[Char]): Boolean = {
    def _for(counter: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty) counter == 0
      else {
        val charHead = chars.head
        if (charHead == ')' && counter == 0) return false
        val newCounter: Int =
          if (charHead == '(') counter + 1
          else if (charHead == ')') counter - 1
          else counter
        _for(newCounter, chars.tail)
      }
    _for(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
