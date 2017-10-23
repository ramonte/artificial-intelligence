import tictactoe.Node
import scala.io.StdIn
import scala.util.Sorting.quickSort

object Tictactoe {
  val X: Int = 101
  val O: Int = 102
  val EMPTY: Int = 103
  val DRAW: Int = 104

  def showItem(item: Int): String = {
    item match {
      case X => return (" X ")
      case O => return (" O ")
      case EMPTY => return (" _ ")
    }
  }

  def printBoard(board: Array[Array[Int]]) = {
    board.foreach((line) => {
      print("\n|")
      line.foreach((item) => {
        print(showItem(item))
        print("|")
      })
      println(" ")
    })
    println("")
  }


  def turnplayer(player: Int): Int = {
    if(player == X) return O else return X
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
  }

  def movementIsValid(x: Int, y: Int, board: Array[Array[Int]]): Boolean = {
    if(x > 3 || y > 3 || x < 1 || y < 1 || board(x-1)(y-1) != EMPTY) return false else return true
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

  def setScore(board: Array[Array[Int]]): Int = {
    if(verifyWin(board) == X) return -1 else if(verifyWin(board) == O) return 1 else return 0
  }

  def generateTree(board: Array[Array[Int]], player: Int): Node = {
    if(verifyWin(board) == EMPTY) {
      return new Node(board, Int.MaxValue, _generateTree(board, turnplayer(player)))
    } else {
      return new Node(board, setScore(board), null)
    }
  }

  def _generateTree(board: Array[Array[Int]], player: Int): Array[Node] = {
    return generateBoards(board, player).map((b) => generateTree(b, player))
  }

  def scorify(root: Node, player: Int): Array[Node] = {
      root.childs.foreach((n) => {
        var value = _scorify(n, turnplayer(player))
        if(n.weight == Int.MaxValue) n.weight = value
        else {
            if(n.weight < value) n.weight = value
        }
      })

      def _scorify(node: Node, player: Int): Int = {
        if(node.childs != null) {
          node.childs.foreach((n) => {
            var value = _scorify(n, turnplayer(player))
            if(n.weight == Int.MaxValue) n.weight = value
            else {
              if(player == X) {
                if(n.weight < value) n.weight = value
              } else {
                if(n.weight > value) n.weight = value
              }
            }
          })
        }
        return node.weight
      }

      return root.childs
  }

  def willWin(node: Node): Boolean = {
    if(verifyWin(node.board) == O) return true else return false
  }

  def willLose(board: Array[Array[Int]]): Array[Array[Int]] = {
    var boards = generateBoards(board, X)
    boards.foreach((b) => {
      if(verifyWin(b) == X) {
        for {
          i <- 0 until 3
          j <- 0 until 3
        } {
          if(b(i)(j) != board(i)(j)) {
            b(i)(j) = O
          }
        }
        return b
      }
    })
    return null
  }

  def getBestBoard(boards: Array[Node]): Array[Array[Int]] = {
    var weight = Int.MinValue
    var board = Array[Array[Int]]()
    boards.foreach((b) => {
      if(b.weight > weight) {
        weight = b.weight
        board = b.board
      }
    })
    return board
  }

  def minimax(board: Array[Array[Int]]): Array[Array[Int]] = {
    var root: Node = generateTree(board, X)
    var nextLevel: Array[Node] = scorify(root, X)

    nextLevel.foreach((b) => {
      if(willWin(b)) return b.board
    })

    var loseScenario = willLose(board)
    if(loseScenario != null) return loseScenario

    return getBestBoard(nextLevel)
  }

  def main(args: Array[String]) = {
    var board: Array[Array[Int]] = Array(Array(EMPTY, EMPTY, EMPTY), Array(EMPTY, EMPTY, EMPTY), Array(EMPTY, EMPTY, EMPTY))
    var player = X
    var win = EMPTY
    printBoard(board)
    while(win == EMPTY) {
      if(player == X) {
        movement(player, board)
      } else {
        board = minimax(board)
      }
      win = verifyWin(board)
      player = turnplayer(player)
      printBoard(board)
    }
    win match {
      case X => println("Jogador venceu!")
      case O => println("Godofredo venceu!")
      case _ => println("Deu velha!")
    }

  }
}
