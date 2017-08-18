import java.awt.{Color, Graphics2D, Dimension}
import swing._

class DataPanel(var data: Array[Array[Color]]) extends Panel {

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
      g.setColor(data(x)(y))
      g.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
  }
}

class Ant() {

}

object Draw extends SimpleSwingApplication {
  /* dimensões e propriedades */
  var alive = 1
  var dead  = 1
  var dim_x = 1
  var dim_y = 1
  var data_aux = Array.ofDim[Color](dim_x, dim_y)

  /* Atribuindo a cor preta pro fundo */
  var data = data_aux.map(_.map(_ => Color.BLACK))
  val rand = scala.util.Random

  /* Inicializando de verdade 
   * @param: dimensão x, dimensão y, formigas vivas, formigas mortas
   */
  def init(x: Int, y: Int, a: Int, d: Int): Unit = {
    dim_x = x
    dim_y = y
    alive = a
    dead = d
    data_aux = Array.ofDim[Color](x,y)
  }

  def definitions(cor: Color): Unit = {
    data = data_aux.map(_.map(_ => Color.BLACK))
    var x = 0
    var y = 0
    for(i: Int <- 0 to dead) {
        x = rand.nextInt(dim_x)
        y = rand.nextInt(dim_y)

        data(x)(y) = cor
    }
  }

  def top = new MainFrame {
    title = "FormiguinhaZ"
    var c = Color.RED
    val timer = new javax.swing.Timer(1000, Swing.ActionListener(e =>
    {
      if(c == Color.RED) {
          definitions(Color.YELLOW)
          c = Color.YELLOW
          contents = new DataPanel(data) {
            preferredSize = new Dimension(600, 600)
          }
        } else {
          definitions(Color.RED)
          c = Color.RED
          contents = new DataPanel(data) {
            preferredSize = new Dimension(600, 600)
          } 
        }
    }))
    timer.start
  }

  override def main(args: Array[String]): Unit = {
    init(Integer.parseInt(args(0)), Integer.parseInt(args(1)), Integer.parseInt(args(2)), Integer.parseInt(args(3)))
    top.open()
  }
}