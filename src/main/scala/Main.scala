
object Main extends App {

  val start = System.currentTimeMillis()

  def nQueens(n: Int) = {

    def tab(a: Int, b: Int) = (a, b)
    val board = Vector.tabulate(n, n)(tab).flatten

    def rowPosition(board: Vector[(Int, Int)], solution: Vector[Int]): Vector[Int] = {
      if (solution.length == n) {
        return solution
      }
      val options = board.filter(tup => tup._1 == solution.length)
      val newBoard = board.filter(entry => entry._1 != solution.length)
      for (coord <- options) {
        val nextRow = rowPosition(newBoard.filter(entry => entry._2 != coord._2 && !diagonalFilter(coord).contains(entry)), solution :+ coord._2)
        if (nextRow.last >= 0) {
          return nextRow
        }
      }
      return solution :+ -1
    }

    def diagonalFilter(coord: (Int, Int)) = {
      var denied: Vector[(Int, Int)] = Vector.empty
      for (a <- 1 to n; b <- 1 to n) {
        denied = denied :+(coord._1 + 1, coord._2 + 1)
        denied = denied :+(coord._1 + 1, coord._2 - 1)
      }
      denied
    }

    rowPosition(board, Vector.empty)
  }

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
