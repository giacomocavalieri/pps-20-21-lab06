package u06lab.code

object TicTacToe extends App {
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  object Winner {
    def unapply(board: Board): Option[Player] = {
      (getRows(board) ++ getColumns(board) ++ getDiagonals(board)) collectFirst {
        case l if l.size == 3 && l.forall(_ == l.head) => l.head
      }
    }
  }

  def getWinner(board: Board): Option[Player] = board match { case Winner(p) => Some(p); case _ => None }

  def getRows(board: Board): List[List[Player]] =
    for (x <- (0 to 2).toList) yield board collect { case Mark(`x`, _, p) => p }

  def getColumns(board: Board): List[List[Player]] =
    for (y <- (0 to 2).toList) yield board collect { case Mark(_, `y`, p) => p }

  def getDiagonals(board: Board): List[List[Player]] =
    List(board collect { case Mark(x, y, p) if x == y => p }, // Diagonal
         board collect { case Mark(x, y, p) if x + y == 2 => p }) // Anti-diagonal

  def find(board: Board, x: Int, y: Int): Option[Player] = board collectFirst { case Mark(`x`, `y`, p) => p }

  def placeAnyMark(board: Board, player: Player): Seq[Board] =
    for (x <- 0 to 2; y <- 0 to 2; if find(board, x, y).isEmpty) yield Mark(x, y, player) :: board

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match {
    case 0 => LazyList(List(Nil))
    case _ =>
      val games = computeAnyGame(player.other, moves - 1)
      val (over, notOver) = games partition (game => getWinner(game.head).isDefined)
      over ++ (for (game <- notOver; nextBoard <- placeAnyMark(game.head, player)) yield nextBoard :: game)
  }

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ".")
      if (x == 2) { print(" "); if (board == game.head) println()}
    }

  // Exercise 1: implement find such that..
  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

  // Exercise 2: implement placeAnyMark such that..
  printBoards(placeAnyMark(List(),X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...
  printBoards(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  //computeAnyGame(O, 8) foreach {g => printBoards(g); println()}
  println(computeAnyGame(O, 6).length)
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
}
