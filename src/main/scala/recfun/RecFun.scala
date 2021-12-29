package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    print("Parentheses Balancing")
    println(" - " + balance("".toList))
    println("test - " + balance("test".toList))
    println("test( - " + balance("test(".toList))
    println("test() - " + balance("test()".toList))
    println("test()) - " + balance("test())".toList))
    println("test())( - " + balance("test())(".toList))

    println("Counting Change")
    println(countChange(100, List(5, 10)))

  /**
   * This method keep an index of rows and columns and check if a point is the first or last value in the row
   * if first or last, point = 1. Else else take position and sum the previous row n & n-1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
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
   * This method counts how many different ways you can make change for an amount
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def _for(amount: Int, coins: List[Int]): Int = {
      if (amount == 0) 1
      else if (amount < 0 || coins.length == 0) 0
      else _for(amount, coins.tail) + _for(amount - coins.head, coins)
    }

    _for(money, coins.distinct)
  }
