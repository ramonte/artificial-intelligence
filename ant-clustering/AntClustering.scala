import java.awt.{Color, Graphics2D, Dimension}
import swing._

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
        case 0 => g.setColor(Color.BLACK)
        case 1 => g.setColor(Color.RED)
        case 2 => g.setColor(Color.WHITE)
        case 3 => g.setColor(Color.GREEN)
      }
      g.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
  }
}

class Ant(val xi: Int, val yi: Int) {
  var x: Int = xi
  var y: Int = yi
  var carrying: Boolean = false
  var above: Boolean = false

  def setCoordinate(i: Int, j: Int): Unit = {
    this.x = i
    this.y = j
  }

}

class DrawUI(val x: Int, val y: Int, val a: Int, val d: Int, val v: Int) extends SimpleSwingApplication {
  /* dimensões e propriedades */
  var alive: Int = a
  var dead: Int = d
  var dim_x: Int = x
  var dim_y: Int = y
  var data: Array[Array[Int]] = null
  var ants: Array[Ant] = null
  var vision: Int = v
  val ncores: Int = Runtime.getRuntime().availableProcessors()

  /* Util */
  val rand = scala.util.Random

  def definitions(): Unit = {
    this.data = Array.ofDim[Int](this.dim_x, this.dim_y)
    this.data.map(_.map(_ => 0))

    this.ants = Array.ofDim[Ant](this.alive)

    var x = 0
    var y = 0
    var idx_ants:Int = 0

    for(i: Int <- 0 until this.dead) {
        do {
          x = rand.nextInt(this.dim_x)
          y = rand.nextInt(this.dim_y)
        } while(this.data(x)(y) == 1)
        this.data(x)(y) = 1
    }

    for(i: Int <- 0 until this.alive) {
      do {
        x = rand.nextInt(this.dim_x)
        y = rand.nextInt(this.dim_y)
      } while(this.data(x)(y) == 2 || this.data(x)(y) == 1);
      this.data(x)(y) = 2
      this.ants(idx_ants) = new Ant(x, y)
      idx_ants += 1
    }
  }

  def top = new MainFrame {
    title = "clustering"
    definitions
    contents = new DataPanel(data) {
        preferredSize = new Dimension(600, 600)
    }
    
    val timer = new javax.swing.Timer(16, Swing.ActionListener(e =>
    {
      this.repaint 
    }))
    timer.start
    start_movements
  }

  def vision_size(): Int = {
    var odd = (((1 to dim_x).toStream).filter(_ % 2 != 0))(vision)
    (odd * odd) - 1
  }

  def start_movements = {
    val ants_perthread = this.alive / ncores
    var actual = 0
    var last = ants_perthread
    for(i: Int <- 0 until ncores) {
      if(last < this.alive) {
        movement(actual, last)
        actual = last + 1
        if(i < (ncores-2)) 
          last += (ants_perthread + 1)
        else 
          last = this.alive-1
      }
    }
  }

  def movement(init: Int, end: Int): Unit = {
    val thread = new Thread {
      override def run {
        Thread.sleep(2000)
        println("início e fim: " + init + " " + end)
        var iterations = 0
        while(iterations < 5000000) {
          for(idx: Int <- init to end) {
            val old_x = ants(idx).x
            val old_y = ants(idx).y 
            var new_x = 0
            var new_y = 0
            
            do {
              new_x = rand.nextInt(3)-1
              new_y = rand.nextInt(3)-1
            } while((new_x == 0 && new_y == 0) || 
                    (old_x + new_x) < 0 || (old_x + new_x) >= dim_x || 
                    (old_y + new_y) < 0 || (old_y + new_y) >= dim_y || 
                    data(old_x + new_x)(old_y + new_y) == 2 ||
                    data(old_x + new_x)(old_y + new_y) == 3 ||
                    (old_x == new_x && old_y == new_y)
              );

            ants(idx).setCoordinate(old_x + new_x, old_y + new_y)

            /* Se estava acima de algum corpo (sem carregá-lo) */
            val was_above = ants(idx).above
            if(!was_above) 
              data(old_x)(old_y) = 0
            else
              data(old_x)(old_y) = 1

            /* Se vai para cima de um novo corpo */
            if(data(ants(idx).x)(ants(idx).y) == 1) {

              /* Se está carregando */
              if(ants(idx).carrying) {
                ants(idx).above = true
              } else {
                pickup(idx)
              }

            }

            /* Se vai para um lugar vazio */
             else {

              if(ants(idx).carrying) {
                drop(idx)
              } else {
                ants(idx).above = false
              }

            }

            /* Difere entre formigas carregando ou não */
            if(ants(idx).carrying) 
              data(ants(idx).x)(ants(idx).y) = 3
            else 
              data(ants(idx).x)(ants(idx).y) = 2
          }

          //Thread.sleep(10)
        }    

        iterations += 1
      }
    }

    /* back to function 'movement' */
    thread.start

  }

  def neighbourhood(idx: Int): Int = {
    var closer = 0
    for(i: Int <- -vision to vision) {
      for(j: Int <- -vision to vision) {
        if( (ants(idx).x + i) >= 0 && (ants(idx).x + i) < dim_x && 
            (ants(idx).y + j) >= 0 && (ants(idx).y + j) < dim_y  
          ) {
            if( data(ants(idx).x + i)(ants(idx).y + j) == 1 )
              closer += 1
            }
      }
    }
    closer
  }

  def pickup(idx: Int): Unit = {
    val closer = neighbourhood(idx)
    val prob_persquare = closer.toFloat / vision_size
    val prob_random: Double = rand.nextDouble()

    if( prob_random >= prob_persquare ) {
      ants(idx).carrying = true
      ants(idx).above = false
    } else {
      ants(idx).above = true
    }
  }

  def drop(idx: Int): Unit = {
    val closer = neighbourhood(idx)
    val prob_persquare: Double = closer.toFloat / vision_size
    val prob_random: Double = rand.nextDouble()

    if( prob_random <= prob_persquare ) {
      ants(idx).carrying = false
      ants(idx).above = true
    } else {
      ants(idx).above = false
    }
  }

  override def main(args: Array[String]): Unit = {
    top.open()
  }
}

object AntClustering {
  def main(args: Array[String]): Unit = {
    if(args.length < 5)
      println("args: x y n_vivas n_mortas raio_visao")
    else {
      println(Runtime.getRuntime().availableProcessors() + " threads")
      var gg = new DrawUI(Integer.parseInt(args(0)), 
                        Integer.parseInt(args(1)), 
                        Integer.parseInt(args(2)), 
                        Integer.parseInt(args(3)),
                        Integer.parseInt(args(4)))
      gg.main(args)
    }
  }
}