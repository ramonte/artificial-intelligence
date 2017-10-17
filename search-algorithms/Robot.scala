import java.awt.{Color, Graphics2D, Dimension}
import swing._
import scala.io.Source
import scala.io.StdIn._
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

class DjNode(val cost: Int, val x: Int, val y: Int) {}

class DrawUI(val x_ini: Int, val y_ini: Int,
             val x_end: Int, val y_end: Int,
             val mode: Int) extends SimpleSwingApplication {
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

  /* BFS search */

  def dothe_bfs = {
    tree = createTree(x_ini, y_ini, getCost(x_ini, y_ini))
    readFile
    val t0 = System.nanoTime()
    var objective: Node = bfs
    val tf = System.nanoTime()
    readFile

    var costs: Array[Array[Int]] = Array.ofDim(DIMENSIONS, DIMENSIONS)
    for {
      i <- 0 until DIMENSIONS
      j <- 0 until DIMENSIONS
    } {
      costs(i)(j) = getCost(i, j)
    }
    var path: Buffer[(Int, Int)] = Buffer()

    var aux = objective
    var nodes: Int = 0
    var cost: Int = 0
    while(aux != null) {
      path.append( (aux.x, aux.y) )
      aux = aux.father
      nodes += 1
    }
    for(i <- path.reverse) {

      data(i._1)(i._2) = VISITED
      cost = cost + costs(i._1)(i._2)
    }

    println("Custo final: " + cost + "\nNós percorridos: " + nodes + "\nTempo de execução (ms): " + (tf-t0)/1e9d)
  }

  def bfs: Node = {
    var open: Buffer[Node] = Buffer(tree)
    while(open.length > 0) {
      var search = open(0)
      open.remove(0)


      if(search.x == x_end && search.y == y_end) {
        return search
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
    return null
  }

  /* Uniform cost search */

  def dothe_uc(is_astar: Boolean) = {
    val t0 = System.nanoTime()
    var costs: Array[Array[DjNode]] = uniform_cost(is_astar)
    val tf = System.nanoTime()
    var path: Buffer[(Int, Int)] = Buffer()

    readFile
    var position: (Int, Int)  = (x_end, y_end)

    while(position != (x_ini, y_ini)) {
      path.append(position)
      position = (costs(position._1)(position._2).x, costs(position._1)(position._2).y)
    }

    for(i <- path.reverse) {

      data(i._1)(i._2) = VISITED
    }
    println("Custo final: " + costs(x_end)(y_end).cost + "\nNós percorridos: " + path.length + "\nTempo de execução (ms): " + (tf-t0)/1e9d)
  }

  def uniform_cost(is_astar: Boolean): Array[Array[DjNode]] = {
    var dj_data = data.map(_.clone)
    var dj_costs: Array[Array[DjNode]] = Array.ofDim[DjNode](DIMENSIONS, DIMENSIONS)
    var dj_list: Buffer[(Int, Int)] = Buffer((x_ini, y_ini))

    var astar_distances: Array[Array[Int]] = null
    var prop1: Double = 1
    var prop2: Double = 1

    if(is_astar) {
      // println("Entre com as proporcoes para o custo e a distancia de manhattan, respectivamente")
      // prop1 = readInt()
      // prop2 = readInt()
      // println(prop1 + " e " + prop2)

      astar_distances = Array.ofDim[Int](DIMENSIONS, DIMENSIONS)
      for {
        i <- 0 until DIMENSIONS
        j <- 0 until DIMENSIONS
      } {
        astar_distances(i)(j) = manhattan(i, j)
      }
    }

    for {
      i <- 0 until DIMENSIONS
      j <- 0 until DIMENSIONS
    } {
      dj_costs(i)(j) = new DjNode(Int.MaxValue, Int.MaxValue, Int.MaxValue)
    }

    dj_costs(x_ini)(y_ini) = new DjNode(0, x_ini, y_ini)
    data(x_ini)(y_ini) = VISITING

    while(dj_list.length > 0) {
      if (is_astar) {
        dj_list = dj_list.sortWith(sortbyCostAStar)
      } else {
        dj_list = dj_list.sortWith(sortbyCost)
      }

      var position = dj_list(0)
      dj_list.remove(0)

      data(position._1)(position._2) = VISITED

      if(position._1 == x_end && position._2 == y_end) {
        return dj_costs
      }



      var positionCost = 0

      if(position._2 - 1 >= 0) {
        expand(position._1, position._2 - 1, position._1, position._2)
      }

      if(position._1 + 1 < DIMENSIONS) {
        expand(position._1 + 1, position._2, position._1, position._2)
      }

      if(position._2 + 1 < DIMENSIONS) {
        expand(position._1, position._2 + 1, position._1, position._2)
      }

      if(position._1 - 1 >= 0) {
        expand(position._1 - 1, position._2, position._1, position._2)
      }

    }


    def sortbyCost(item1: (Int,Int), item2: (Int,Int)) = {
      dj_costs(item2._1)(item2._2).cost > dj_costs(item1._1)(item1._2).cost
    }

    def sortbyCostAStar(item1: (Int, Int), item2: (Int, Int)) = {
      (dj_costs(item2._1)(item2._2).cost * prop1 + astar_distances(item2._1)(item2._2)*prop2) > (dj_costs(item1._1)(item1._2).cost * prop1 + astar_distances(item1._1)(item1._2) * prop2)
    }

    def manhattan(x: Int, y: Int): Int = {
      (Math.abs(x - x_end) + Math.abs(y - y_end))
    }

    def getCostDj(x: Int, y: Int): Int = {
      var cost: Int = 0
      dj_data(x)(y) match {
        case GRASS => cost = 1
        case MOUNTAIN => cost = 5
        case SWAMP => cost = 10
        case FIRE => cost = 15
      }
      cost
    }

    def expand(x: Int, y: Int, prev_x: Int, prev_y: Int) = {
      var positionCost = getCostDj(x, y)
      if(dj_costs(x)(y).cost > dj_costs(prev_x)(prev_y).cost + positionCost) {
        dj_costs(x)(y) = new DjNode(dj_costs(prev_x)(prev_y).cost + positionCost, prev_x, prev_y)
        if(!dj_list.contains((x,y))) dj_list.append((x,y))
        data(x)(y) = VISITING
      }
    }

    return dj_costs
  }

  /* Iteractive deepening search */

  def dothe_ids = {
    tree = createTree(x_ini, y_ini, getCost(x_ini, y_ini))
    readFile
    val t0 = System.nanoTime()
    var objective: Node = ids
    val tf = System.nanoTime()
    readFile

    var costs: Array[Array[Int]] = Array.ofDim(DIMENSIONS, DIMENSIONS)
    for {
      i <- 0 until DIMENSIONS
      j <- 0 until DIMENSIONS
    } {
      costs(i)(j) = getCost(i, j)
    }
    var path: Buffer[(Int, Int)] = Buffer()

    var aux = objective
    var nodes: Int = 0
    var cost: Int = 0
    while(aux != null) {
      path.append( (aux.x, aux.y) )
      aux = aux.father
      nodes += 1
    }
    for(i <- path.reverse) {

      data(i._1)(i._2) = VISITED
      cost = cost + costs(i._1)(i._2)
    }
    println("Custo final: " + cost + "\nNós percorridos: " + nodes + "\nTempo de execução (ms): " + (tf-t0)/1e9d)
  }

  def ids: Node = {
    var original_data: Array[Array[Int]] = data.map(_.clone)
    var level: Int = 1
    var objective: Node = null

    while(objective == null) {
      objective = deepening_search(tree, 1, level)
      level = level + 1
      readFile
    }
    return objective
  }

  def deepening_search(node: Node, level: Int, maxlevel: Int): Node = {
    if(node.x == x_end && node.y == y_end) {
      return node
    }

    if(level == maxlevel) {
      return null
    }

    Thread.sleep(5)

    var objective: Node = null

    if(node.n != null) {
      data(node.n.x)(node.n.y) = VISITING
      var result: Node = deepening_search(node.n, level+1, maxlevel)
      if(result != null) {
        objective = result
      }
    }
    if(node.e != null) {
      data(node.e.x)(node.e.y) = VISITING
      var result: Node = deepening_search(node.e, level+1, maxlevel)
      if(result != null) {
        objective = result
      }
    }
    if(node.s != null) {
      data(node.s.x)(node.s.y) = VISITING
      var result: Node = deepening_search(node.s, level+1, maxlevel)
      if(result != null) {
        objective = result
      }
    }
    if(node.w != null) {
      data(node.w.x)(node.w.y) = VISITING
      var result: Node = deepening_search(node.w, level+1, maxlevel)
      if(result != null) {
        objective = result
      }
    }

    data(node.x)(node.y) = VISITED


    return objective
  }

  override def main(args: Array[String]): Unit = {
    data = Array.ofDim[Int](DIMENSIONS, DIMENSIONS)
    readFile
    top.open
    mode match {
      case 1 => dothe_bfs
      case 2 => dothe_uc(false)
      case 3 => dothe_ids
      case 4 => dothe_uc(true)
      case _ => println("Modo inválido.")
    }
  }

}

object Robot {
  def main(args: Array[String]): Unit = {
    if(args.length == 5) {
      var obj = new DrawUI(Integer.parseInt(args(0)),
                           Integer.parseInt(args(1)),
                           Integer.parseInt(args(2)),
                           Integer.parseInt(args(3)),
                           Integer.parseInt(args(4)))
      obj.main(null)
    } else {
      println("args: x_ini y_ini x_end y_end option")
    }
  }
}
