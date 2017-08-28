import java.awt.{Color, Graphics2D, Dimension}
import swing._
import scala.io.Source

class DataPanel(var data: Array[Array[Info]]) extends Panel {

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
      if(data(x)(y) != null) {
        g.setColor(data(x)(y).color)
      } else {
        g.setColor(Color.BLACK)
      }
      g.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
  }
}

class Info(val att: List[Double], val t: Int) {
  var attributes: List[Double] = att
  var group: Int = t
  var color: Color = Color.BLACK
}

class Ant(val xi: Int, val yi: Int) {
  var x: Int = xi
  var y: Int = yi
  var carrying: Info = null

  def setCoordinate(i: Int, j: Int): Unit = {
    this.x = i
    this.y = j
  }
}

class DrawUI(val x: Int, val y: Int, val a: Int, val d: Int, val v: Int, val f: String, val s: String) extends SimpleSwingApplication {
  /* dimensões e propriedades */
  var alive: Int = a
  var dead: Int = d
  var dim_x: Int = x
  var dim_y: Int = y
  var data: Array[Array[Info]] = null
  var ants: Array[Ant] = null
  var vision: Int = v
  var filename: String = f
  var separator: String = s

  /* Util */
  val rand = scala.util.Random

  def definitions(): Unit = {
    this.data = Array.ofDim[Info](this.dim_x, this.dim_y)
    readFile

    this.ants = Array.ofDim[Ant](this.alive)
    var x = 0
    var y = 0

    for(i: Int <- 0 until this.alive) {
      do {
        x = rand.nextInt(this.dim_x)
        y = rand.nextInt(this.dim_y)
      } while(this.data(x)(y) != null);
      this.ants(i) = new Ant(x, y)
    }
  }

  def top = new MainFrame {
    title = "clustering"
    contents = new DataPanel(data) {
        preferredSize = new Dimension(600, 600)
    }

    val timer = new javax.swing.Timer(16, Swing.ActionListener(e =>
    {
      this.repaint
    }))
    timer.start
  }

  def vision_size(): Int = {
    var odd = (((1 to dim_x).toStream).filter(_ % 2 != 0))(vision)
    (odd * odd) - 1
  }

  def generateColors(ncolors: Int): Array[Color] = {
    var colors: Array[Color] = Array.ofDim[Color](ncolors)
    for(i: Int <- 0 until ncolors) {
      val r = rand.nextInt(256)
      val g = rand.nextInt(256)
      val b = rand.nextInt(256)
      colors(i) = new Color(r,g,b)
    }
    colors
  }

  def start_movements = {
    for(i: Int <- 0 until alive) {
      movements(i)
    }
  }

  def movements(idx: Int): Unit = {
    val thread = new Thread {
      override def run {
        for(i: Int <- 0 to 5000000) {
          move(idx)
        }
        while(ants(idx).carrying != null) {
          move(idx)
        }
        println("ant number " + idx + " says: Done!")
      }
    }
    thread.start
  }

  def move(idx: Int): Unit = {
    val old_x = ants(idx).x
    val old_y = ants(idx).y
    var new_x = 0
    var new_y = 0
    var x = 0
    var y = 0

    do {

      new_x = rand.nextInt(3)-1
      new_y = rand.nextInt(3)-1
      x = old_x + new_x
      y = old_y + new_y
      if(x == dim_x) x = 0
      if(y == dim_y) y = 0
      if(x == -1) x = (dim_x - 1)
      if(y == -1) y = (dim_y - 1)

    } while((new_x == 0 && new_y == 0));

    ants(idx).setCoordinate(x, y)

    if(data(ants(idx).x)(ants(idx).y) == null) {
      if(ants(idx).carrying != null) {
        drop(idx)
      }
    } else {
      if(ants(idx).carrying == null) {
        pickup(idx)
      }
    }

  }

  def neighbourhood(idx: Int, group: Int): Int = {
    var closer = 0
    for(i: Int <- -vision to vision) {
      for(j: Int <- -vision to vision) {
        var x = ants(idx).x + i
        var y = ants(idx).y + j

        if(x >= dim_x) x = x - dim_x
        if(y >= dim_y) y = y - dim_y

        if(x < 0) x = x + dim_x
        if(y < 0) y = y + dim_y
        if(data(x)(y) != null) {
          if( data(x)(y).group == group ){
            closer += 1
          }
        }
      }
    }
    closer
  }

  def pickup(idx: Int): Unit = {
    val closer = neighbourhood(idx, data(ants(idx).x)(ants(idx).y).group)
    val prob_persquare = closer.toFloat / vision_size
    val prob_random: Double = rand.nextDouble()
    this.synchronized {
      if( prob_random >= prob_persquare && data(ants(idx).x)(ants(idx).y) != null) {
        ants(idx).carrying = data(ants(idx).x)(ants(idx).y)
        data(ants(idx).x)(ants(idx).y) = null
      }
    }
  }

  def drop(idx: Int): Unit = {
    val closer = neighbourhood(idx, ants(idx).carrying.group)
    val prob_persquare = (closer.toFloat / vision_size)
    val prob_random: Double = rand.nextDouble()
    this.synchronized {
      if( prob_random <= prob_persquare && data(ants(idx).x)(ants(idx).y) == null) {
        data(ants(idx).x)(ants(idx).y) = ants(idx).carrying
        ants(idx).carrying = null
      }
    }
  }

  def readFile(): Unit = {
    var ncolors: Int = 0
    for(line <- Source.fromFile(filename).getLines) {
      var att: List[Double] = null
      var x = 0
      var y = 0

      if(!line.startsWith("#")) {
        if(!line.equals("")) {
          val splitted = line.split(separator)

          for(i: Int <- 0 until splitted.length - 1) {
            att :: List(splitted(i))
          }

          do {
            x = rand.nextInt(dim_x)
            y = rand.nextInt(dim_y)
          } while(data(x)(y) != null)

          data(x)(y) = new Info(att, Integer.parseInt(splitted.last))
          if(Integer.parseInt(splitted.last) > ncolors) ncolors = Integer.parseInt(splitted.last)
        }
      }
    }
    val colors: Array[Color] = generateColors(ncolors)
    for(i: Int <- 0 until ncolors) {
      data.map(_.filter(_ != null).filter(_.group == i+1).map(_.color = colors(i)))
    }
  }

  override def main(args: Array[String]): Unit = {
    definitions
    top.open()
    Thread.sleep(2000)
    start_movements
  }
}

object AntClustering {
  def main(args: Array[String]): Unit = {
    if(args.length < 5)
      println("args: dim_x dim_y n_vivas n_mortas raio_visao nome_arquivo separador_informações")
    else {
      var gg = new DrawUI(Integer.parseInt(args(0)),
                        Integer.parseInt(args(1)),
                        Integer.parseInt(args(2)),
                        Integer.parseInt(args(3)),
                        Integer.parseInt(args(4)),
                        args(5),
                        args(6))
      gg.main(args)
    }
  }
}
