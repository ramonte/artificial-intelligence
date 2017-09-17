import java.awt.{Color, Graphics2D, Dimension}
import swing._
import scala.io.Source
import scala.collection.mutable.Buffer

class DataPanel(var data: Array[Array[Int]]) extends Panel {

  override def paintComponent(g: Graphics2D) {
    val dx = g.getClipBounds.width.toFloat  / data.length
    val dy = g.getClipBounds.height.toFloat / data.map(_.length).max
    for {
      x <- 0 until data.length
      y <- 0 until data(x).length
      x1 = (x * dx).toInt
      y1 = (y * dy).toInt
      x2 = ((x + 1) * dx).toInt
      y2 = ((y + 1) * dy).toInt
    } {
  		data(x)(y) match {
        case 0 => g.setColor(Color.GREEN)
        case 1 => g.setColor(new Color(92,51,23))
        case 2 => g.setColor(Color.BLUE)
        case 3 => g.setColor(Color.RED)
        case 5 => g.setColor(Color.WHITE)
        case 6 => g.setColor(Color.BLACK)
  		}
      g.fillRect(y1, x1, y2 - y1, x2 - x1)
    }
  }
}

class Node(val x: Int, val y: Int, val cost: Int) {
  var n: Node = null
  var e: Node = null
  var s: Node = null
  var w: Node = null
  var father: Node = null
}

class DrawUI(val x_ini: Int, val y_ini: Int,
             val x_end: Int, val y_end: Int) extends SimpleSwingApplication {
  val GRASS: Int = 0
  val MOUNTAIN: Int = 1
  val SWAMP: Int = 2
  val FIRE: Int = 3
  val VISITED: Int = 5
  val VISITING: Int = 6
  val DIMENSIONS: Int = 42

  var data: Array[Array[Int]] = null
  var tree: Node = null

  def top = new MainFrame {
    title = "adventure-robot"
    contents = new DataPanel(data) {
        preferredSize = new Dimension(600, 600)
    }

    val timer = new javax.swing.Timer(16, Swing.ActionListener(e =>
    {
      this.repaint
    }))

    tree = createTree(x_ini, y_ini, getCost(x_ini, y_ini))
    readFile

    timer.start
  }

  def readFile = {
    var i = 0
    for(line <- Source.fromFile("scenario").getLines) {
      val splitted = line.split(" ")
      for(j: Int <- 0 until splitted.length) {
        data(i)(j) = Integer.parseInt(splitted(j))
      }
      i = i + 1
    }
  }

/*
  def createTree(x: Int, y: Int, cost: Int): Node = {
    data(x)(y) = VISITED
    if(x != x_end && y != y_end)
      expand(new Node(x, y, cost))
    else
      new Node(x, y, cost, null, null, null, null)
  }

  def expand(node: Node): Node = {
    def expand_north: Node = {
      if(node.y - 1 >= 0) {
        if(data(node.x)(node.y-1) != VISITED) createTree(node.x, node.y - 1, node.cost + getCost(node.x, node.y - 1))
        else null
      }
      else null
    }

    def expand_south: Node = {
      if(node.y + 1 < DIMENSIONS) {
        if(data(node.x)(node.y + 1) != VISITED) createTree(node.x, node.y + 1, node.cost + getCost(node.x, node.y + 1))
        else null
      }
      else null
    }

    def expand_east: Node = {
      if(node.x + 1 < DIMENSIONS) {
        if(data(node.x + 1)(node.y) != VISITED) createTree(node.x + 1, node.y, node.cost + getCost(node.x + 1, node.y))
        else null
      }
      else null
    }

    def expand_west: Node = {
      if(node.x - 1 >= 0) {
        if(data(node.x - 1)(node.y) != VISITED) createTree(node.x - 1, node.y, node.cost + getCost(node.x - 1, node.y))
        else null
      }
      else null
    }

    node.n = expand_north
    node.l = expand_east
    node.s = expand_south
    node.o = expand_west

    node
  }
  */

  def createTree(x: Int, y: Int, cost: Int): Node = {
    def expand_north(node: Node): Node = {
      if(node.y - 1 >= 0) {
        if(data(node.x)(node.y-1) != VISITED) new Node(node.x, node.y - 1, node.cost + getCost(node.x, node.y - 1))
        else null
      } else {
        null
      }
    }

    def expand_south(node: Node): Node = {
      if(node.y + 1 < DIMENSIONS) {
        if(data(node.x)(node.y + 1) != VISITED) new Node(node.x, node.y + 1, node.cost + getCost(node.x, node.y + 1))
        else null
      } else {
        null
      }
    }

    def expand_east(node: Node): Node = {
      if(node.x + 1 < DIMENSIONS) {
        if(data(node.x + 1)(node.y) != VISITED) new Node(node.x + 1, node.y, node.cost + getCost(node.x + 1, node.y))
        else null
      } else {
        null
      }
    }

    def expand_west(node: Node): Node = {
      if(node.x - 1 >= 0) {
        if(data(node.x - 1)(node.y) != VISITED) new Node(node.x - 1, node.y, node.cost + getCost(node.x - 1, node.y))
        else null
      } else {
        null
      }
    }

    var crtree: Buffer[Node] = Buffer()
    var root = new Node(x, y, cost)
    data(x)(y) = VISITED
    crtree.append(root)

    while(crtree.length > 0) {
      var node = crtree(0)
      crtree.remove(0)

      node.n = expand_north(node)
      node.e = expand_east(node)
      node.s = expand_south(node)
      node.w = expand_west(node)


      if(node.n != null) {
        node.n.father = node
        crtree.append(node.n)
        data(node.n.x)(node.n.y) = VISITED
      }
      if(node.e != null) {
        node.e.father = node
        crtree.append(node.e)
        data(node.e.x)(node.e.y) = VISITED
      }
      if(node.s != null) {
        node.s.father = node
        crtree.append(node.s)
        data(node.s.x)(node.s.y) = VISITED
      }
      if(node.w != null) {
        node.w.father = node
        crtree.append(node.w)
        data(node.w.x)(node.w.y) = VISITED
      }
    }
    root
  }

  def getCost(x: Int, y: Int): Int = {
    var cost: Int = 0
    data(x)(y) match {
      case GRASS => cost = 1
      case MOUNTAIN => cost = 5
      case SWAMP => cost = 10
      case FIRE => cost = 15
    }
    cost
  }

  def bfs: Node = {
    var open: Buffer[Node] = Buffer(tree)
    var choice: Node = null
    var min: Int = 999999
    while(open.length > 0) {
      var search = open(0)
      open.remove(0)

      Thread.sleep(5)
      if(search.x == x_end && search.y == y_end) {
        if(search.cost < min) {
          println(search.cost)
          choice = search
          min = search.cost
        }
      } else {
        if(search.n != null) {
          open.append(search.n)
          data(search.n.x)(search.n.y) = VISITING
        }

        if(search.e != null) {
          open.append(search.e)
          data(search.e.x)(search.e.y) = VISITING
        }

        if(search.s != null) {
          open.append(search.s)
          data(search.s.x)(search.s.y) = VISITING
        }

        if(search.w != null) {
          open.append(search.w)
          data(search.w.x)(search.w.y) = VISITING
        }

      }
      data(search.x)(search.y) = VISITED
    }
    return choice
  }

  override def main(args: Array[String]): Unit = {
    data = Array.ofDim[Int](DIMENSIONS, DIMENSIONS)
    readFile
    top.open
    var ob: Node = bfs
    readFile
    var path: Buffer[(Int, Int)] = Buffer()
    var aux = ob
    var nodes: Int = 0
    while(aux != null) {
      path.append( (aux.x, aux.y) )
      aux = aux.father
      nodes += 1
    }
    for(i <- path.reverse) {
      Thread.sleep(50)
      data(i._1)(i._2) = VISITED
    }
    println("Nós: " + nodes)
  }
}

object Robot {
  def main(args: Array[String]): Unit = {
    if(args.length == 4) {
      var obj = new DrawUI(Integer.parseInt(args(0)),
                           Integer.parseInt(args(1)),
                           Integer.parseInt(args(2)),
                           Integer.parseInt(args(3)))
      obj.main(null)
    } else {
      println("args: x_ini y_ini x_end y_end")
    }
  }
}
