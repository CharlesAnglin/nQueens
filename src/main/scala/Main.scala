
object Main extends App {

  //Start timer
  val start = System.currentTimeMillis()

  //Algorithm summary: there must exist one queen on each row, we use a recursive function (rowPosition) which focuses
  // on finding a legal place for a queen on a given row. It then filters "illegal" coordinates from the board
  // collection and recalls its self to repeat the same actions on the next row. Solution will be in the form
  // Vector[Int] where the index of an Int is the row and the value of the Int gives the column of the queen.
  def nQueens(n: Int) = {

    //Create a collection of all coordinates between (0,0) and (n,n).
    def tab(a: Int, b: Int) = (a, b)
    val board = Vector.tabulate(n, n)(tab).flatten

    //Recursive function mentioned in summary.
    def rowPosition(board: Vector[(Int, Int)], solution: Vector[Int]): Vector[Int] = {
      //We have a legitimate solution when our solution vector is of length n.
      if (solution.length == n) {
        return solution
      }
      //We're going to place a queen on row n, the options value will be the legal coordinates on row n that we can use,
      // so we filter out all coordinates which are not on row n.
      val options = board.filter(tup => tup._1 == solution.length)
      //As we're going to place a queen on row n, coordinates on row n will be illegal for future placements. We filter
      // out coordinates from the board which are on row n. We do it before the for loop in interests of efficiency.
      val newBoard = board.filter(entry => entry._1 != solution.length)
      //Iterate through our possible placements on row n of the queen.
      for (coord <- options) {
        //Call our recursive function to see if a solution exists with this placement of the queen on row n. We also
        // filter out the squares directly below and diagonal to our queen and append our queens coordinates to the solution.
        val nextRow = rowPosition(newBoard.filter(entry => entry._2 != coord._2 && !diagonalFilter(coord).contains(entry)), solution :+ coord._2)
        //If a solution doesn't exist -1 will be appended to the returned solution value, if the last value is positive we have a valid solution
        if (nextRow.last >= 0) {
          return nextRow
        }
      }
      //If after iterating through each possible placement of the queen on row n we find no valid solutions we append -1
      // to the solution variable we return to tell the n-1 th row that there exist no valid solutions and the queen on the n-1 th needs to be moved.
      return solution :+ -1
    }

    //Given the coordinate of a queen, returns a vector of all diagonal "illegal" squares so we can filter "illegal" spaces out.
    def diagonalFilter(coord: (Int, Int)) = {
      var denied: Vector[(Int, Int)] = Vector.empty
      for (a <- 1 to n; b <- 1 to n) {
        denied = denied :+(coord._1 + 1, coord._2 + 1)
        denied = denied :+(coord._1 + 1, coord._2 - 1)
      }
      denied
    }

    //Start solution search.
    rowPosition(board, Vector.empty)
  }

  //Function to format and print out elapsed time.
  def elapsedTime(start: Long) = {
    val totalTime = System.currentTimeMillis - start
    if (totalTime < 1000) {
      println("Elapsed time: " + totalTime + " ms")
    } else if (totalTime < 60000) {
      println("Elapsed time: " + totalTime / 1000 + " sec, " + (totalTime - (totalTime / 1000) * 1000) + " ms")
    } else {
      println("Elapsed time: " + totalTime / 60000 + " min, " + (totalTime - (totalTime / 60000) * 60000) / 1000 + " sec, " + (totalTime - (totalTime / 1000) * 1000 + " ms"))
    }
  }




  println("solution: " + nQueens(50))

  elapsedTime(start)

  // 50 in 2 sec, 898 ms (on home laptop)
  // 100 in 3 min, 6 sec, 262 ms (on home laptop)


}
