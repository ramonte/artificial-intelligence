import scala.collection.mutable.Buffer
import scala.io.StdIn

object Tictactoe {
  val X: Int = 101
  val O: Int = 102
  val EMPTY: Int = 103
  val DRAW: Int = 104

  def printBoard(board: Array[Array[Int]]) = {
    board.foreach((line) => {
      print("\n|")
      line.foreach((item) => {
        item match {
          case X => print(" X ")
          case O => print(" O ")
          case EMPTY => print(" _ ")
        }
        print("|")
      })
      println(" ")
    })
    println("")
  }

  def getWinConditions(board: Array[Array[Int]]): Array[Array[Int]] = {
    val transposed: Array[Array[Int]] = board.transpose
    return (board ++ transposed ++  Array(Array(board(0)(0), board(1)(1), board(2)(2))) ++ Array(Array(board(0)(2), board(1)(1), board(2)(0))))
  }

  def verifyWin(board: Array[Array[Int]]): Int ={
    var draw = true
    getWinConditions(board).foreach((line) => {
      if(line(0) == line(1) && line(1) == line(2) && line(2) != EMPTY) return line(2)
      if(line(0) == EMPTY || line(1) == EMPTY || line(2) == EMPTY) draw = false
    })
    if(draw) return DRAW else return EMPTY
  }

  def movement(player: Int, board: Array[Array[Int]]) = {
    var x: Int = 0
    var y: Int = 0
    do {
      println("Insira a linha e a coluna (1 a 3): ")
      x = StdIn.readInt()
      y = StdIn.readInt()
      if(!movementIsValid(x, y, board)) println("Movimento inválido!")
    } while(!movementIsValid(x, y, board))

    board(x-1)(y-1) = player
    printBoard(board)
  }

  def movementIsValid(x: Int, y: Int, board: Array[Array[Int]]): Boolean = {
    if(x > 3 || y > 3 || x < 1 || y < 1 || board(x-1)(y-1) != EMPTY) return false else return true
  }

  def setScore(board: Array[Array[Int]]): Int = {
    if(verifyWin(board) == X) return -1 else if(verifyWin(board) == O) return 1 else return 0
  }

  def generateBoards(board: Array[Array[Int]], player: Int): Array[Array[Array[Int]]] = {
    var allBoards: Array[Array[Array[Int]]] = Array[Array[Array[Int]]]()
    for {
      i <- 0 until 3
      j <- 0 until 3
    } {
      var newBoard = _generateBoards(player, i, j)
      if(newBoard != null) {
        allBoards = allBoards ++ Array(newBoard)
      }
    }

    def _generateBoards(_player: Int, _x: Int, _y: Int): Array[Array[Int]] = {
      if(board(_x)(_y) == EMPTY) {
        var _board: Array[Array[Int]] = board.map(_.clone)
        _board(_x)(_y) = _player
        return _board
      } else {
        return null
      }
    }

    return allBoards
  }

  def main(args: Array[String]) = {
    var board: Array[Array[Int]] = Array(Array(EMPTY, EMPTY, EMPTY), Array(EMPTY, EMPTY, EMPTY), Array(EMPTY, EMPTY, EMPTY))
    var player = X
    generateBoards(board, player)
    printBoard(board)
    var win = EMPTY
    while(win == EMPTY) {
      movement(player, board)
      win = verifyWin(board)
      if(player == X) player = O else player = X
    }
  }
}
