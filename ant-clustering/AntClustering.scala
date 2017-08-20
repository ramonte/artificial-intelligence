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
        x = rand.nextInt(this.dim_x)
        y = rand.nextInt(this.dim_y)

        this.data(x)(y) = 1
    }
    for(i: Int <- 0 until this.alive) {
      do {
        x = rand.nextInt(this.dim_x)
        y = rand.nextInt(this.dim_y)

      } while(this.data(x)(y) == 2);
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
    movement
    val timer = new javax.swing.Timer(10, Swing.ActionListener(e =>
    {
      this.repaint 
    }))
    timer.start
  }

  def visionSize(): Int = {
    var odd = (((1 to dim_x).toStream).filter(_ % 2 != 0))(vision)
    (odd * odd) - 1
  }

  def pminimax(idx: Int): Array[Array[Int]] = {
    var auxx: Array[Array[Int]] = Array.ofDim[Int]( (((3 to 50).toStream).filter(_ % 2 != 0))(vision-1), (((3 to 50).toStream).filter(_ % 2 != 0))(vision-1) )
    auxx.map(_.map(_ => 0))
    val anchor = auxx.length/2

    val old_x = ants(idx).x
    val old_y = ants(idx).y 
    var n = 0
    var m = 0
    for(i: Int <- old_x-vision to old_x+vision) {
      for(j: Int <- old_y-vision to old_y+vision) {
        if(i >= 0 && i < dim_x && j >= 0 && j < dim_y) {
          if(data(i)(j) != 2)
            auxx(n)(m) = data(i)(j)
          m += 1
        }
      }
      n += 1
      m = 0
    }

    for(i: Int <- 0 until auxx.length) {
      var x = 0
      var y = 0

      if(i < anchor) {
        if(i + 1 != anchor)
          x = i + 1
        else 
          x = i
      } else if(i > anchor){
        if(i-1 != anchor)
          x = i - 1
        else 
          x = i
      } else {
        x = i
      }

      for(j: Int <- 0 until auxx.length) {
        if(auxx(i)(j) == 1) {
          if(j < anchor) {
            if(j+1 != anchor)
              y = j + 1
            else
              y = j
          } else if(j > anchor){
            if(j-1 != anchor)
              y = j - 1
            else
              y = j
          } else {
            y = j
          }
          auxx(x)(y) += 1
        }
      }
    }

    auxx
  }

  def movement = {
    val thread = new Thread {
      override def run {
        Thread.sleep(2000)
        while(true) {
          for(idx: Int <- 0 until alive) {
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
                    data(old_x + new_x)(old_y + new_y) == 3
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
                var closer = count_neighbour(idx)

                val prob_persquare: Double = 1.0/ ((visionSize/vision)- (1/vision))
                val prob_random: Double = rand.nextDouble()
                //val prob_random: Int = rand.nextInt(visionSize)

                if( prob_random >= prob_persquare*closer ) {
                //if( ((visionSize) -closer) >= prob_random) {
                  ants(idx).carrying = true
                  ants(idx).above = false
                } else {
                  ants(idx).above = true
                }
              }
            }

            /* Se vai para um lugar vazio */
             else {

              if(ants(idx).carrying) {

                val closer = count_neighbour(idx)

                val prob_persquare: Double = 1.0/ ((visionSize/vision)- (1/vision))
                val prob_random: Double = rand.nextDouble()
                //val prob_random: Int = rand.nextInt(visionSize)

                if( prob_random <= prob_persquare*closer ) {
                //if( ((visionSize) -closer) <= prob_random) {
                  ants(idx).carrying = false
                  ants(idx).above = true
                } else {
                  ants(idx).above = false
                }
              } else {
                ants(idx).above = false
              }
            }

            if(ants(idx).carrying) 
              data(ants(idx).x)(ants(idx).y) = 3
            else 
              data(ants(idx).x)(ants(idx).y) = 2
          }

          //Thread.sleep(10)
        }    

        def count_neighbour(idx: Int): Int = {
          var closer = 0
          for(i: Int <- -vision to vision) {
            for(j: Int <- -vision to vision) {
              if( (ants(idx).x + i) >= 0 && (ants(idx).x + i) < dim_x && 
                  (ants(idx).y + j) >= 0 && (ants(idx).y + j) < dim_y  
                ) {
                  if( data(ants(idx).x + i)(ants(idx).y + j) == 1 && i != 0 && j != 0 )
                    closer += 1
                  }
            }
          }
          closer
        }
      }
    }

    /* back to function 'movement' */
    thread.start
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
      var gg = new DrawUI(Integer.parseInt(args(0)), 
                        Integer.parseInt(args(1)), 
                        Integer.parseInt(args(2)), 
                        Integer.parseInt(args(3)),
                        Integer.parseInt(args(4)))
      gg.main(args)
    }
  }
}