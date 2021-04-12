package u06lab.code

import org.junit.jupiter.api.Test
import TicTacToe._
import org.junit.jupiter.api.Assertions._

object TicTacToeTest {
  val board: Board = List(Mark(0, 0, O), Mark(0, 1, O), Mark(0, 2, O),
                          Mark(1, 0, X), Mark(1, 1, X), Mark(1, 2, O),
                          Mark(2, 0, X), Mark(2, 1, X), Mark(2, 2, O))
  val rows = for (x <- 0 to 2) yield board collect { case Mark(`x`, _, p) => p }
  val cols = for (y <- 0 to 2) yield board collect { case Mark(_, `y`, p) => p }
  val diags = List(board collect { case Mark(x, y, p) if x == y => p },
                   board collect { case Mark(x, y, p) if x + y == 2 => p })

  @Test def testGetRows(): Unit = assertEquals(rows, getRows(board))

  @Test def testGetColumns(): Unit = assertEquals(cols, getColumns(board))

  @Test def testGetDiagonals(): Unit = assertEquals(diags, getDiagonals(board))

  @Test def testGetWinner(): Unit = assertEquals(Some(O), getWinner(board))

  @Test def testComputeGamesGeneratesCorrectNumber(): Unit = {
    assertEquals(9*8*7, computeAnyGame(X, 3).size)
    // According to http://www.se16.info/hgb/tictactoe.htm the total number of games
    // (taking into account games ending prematurely due to a player winning) is 255168...
    assertEquals(255168, computeAnyGame(X, 9).size)
  }
}
