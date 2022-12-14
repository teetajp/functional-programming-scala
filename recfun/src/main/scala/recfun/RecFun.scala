package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    // Each number inside the triangle is the sum of the two numbers above it
    // The number of columns in each row is equal to the row index + 1
    // Out of bounds -> 0
    // pascal(c, r) = pascal (c, r-1) + pascal(c, r-1)
    if c < 0 || r < 0 then 0
    else if c == 0 || c == r then 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    // Number of ( should always be greater or equal to number of than ) at any point before the end.
    // At the end, number of ( should be equal to number of )
    // Return false is number of ) is greater than number of ( at any time, and check again at the end to ensure equality
    def balanceRecur(numOpen: Int, numClose: Int, chars: List[Char]): Boolean =
      if chars.isEmpty then numOpen == numClose
      else if numClose > numOpen then false
      else if chars.head == '(' then balanceRecur(numOpen + 1, numClose, chars.tail)
      else if chars.head == ')' then balanceRecur(numOpen, numClose + 1, chars.tail)
      else balanceRecur(numOpen, numClose, chars.tail)

    balanceRecur(0, 0, chars)
  

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    // Number of ways to give change for $money$ amount, using the given list of coin denominations
    
    // Algorithm:
    // use a set to keep track of the money parameters we have computed to avoid duplicates
    // Add each $money$ param, we will call countChange on money-coins[i] for each coin <= money
    // Add 1 when $money$ is 0
    // Our recursion case is as follows: countChange(money - coins[0], coins) + countchange(money, coins[1 ... n-1])
    // In the part of the recursive case, we subtract the current denom
    // The second part of the recursive case calculates countChange using the rest of the coins
    // This way, we try subtract the current denomination until we exhaust it, but also at each iteration,
    // We try countingChange using other denominations as well
    // Because we are doing this in order of denominations, we avoid duplicates
    if money == 0 then 1 // valid, exact change
    else if money < 0 || coins.isEmpty then 0 // invalid
    else countChange(money - coins.head, coins) + countChange(money, coins.tail) // recursive case
