import java.awt.{Color, Graphics2D, Dimension}
import swing._
import scala.io.Source
import scala.collection.mutable.Buffer

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
    		data(x)(y).group match {
    			case 1 =>  g.setColor(new Color(255, 0, 0))
                case 2 =>  g.setColor(new Color(0, 255, 0))
                case 3 =>  g.setColor(new Color(0, 0, 255))
                case 4 =>  g.setColor(new Color(255, 255, 0))
                case 5 =>  g.setColor(new Color(255, 0, 255))
                case 6 =>  g.setColor(new Color(0, 255, 255))
                case 7 =>  g.setColor(new Color(100, 0, 0))
                case 8 =>  g.setColor(new Color(100, 100, 0))
                case 9 =>  g.setColor(new Color(0, 100, 0))
                case 10 => g.setColor(new Color(100, 0, 100))
                case 11 => g.setColor(new Color(0, 0, 100))
                case 12 => g.setColor(new Color(0, 100, 100))
                case 13 => g.setColor(new Color(255, 100, 0))
                case 14 => g.setColor(new Color(100, 255, 100))
                case 15 => g.setColor(new Color(100, 250, 255))
	    		case _ => g.setColor(Color.BLACK)
    		}
    	} else {
    		g.setColor(Color.BLACK)
    	}
      	g.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
  }
}

class Info(val att: Buffer[Double], val t: Int) {
  var attributes: Buffer[Double] = att
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

class DrawUI(val x: Int, val y: Int, val a: Int, val d: Int, val v: Int, val f: String, val s: String, val al: Double) extends SimpleSwingApplication {
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

  // T1 val alpha: Double = 33
  // T2 val alpha: Double = 0.9
  val alpha: Double = al
  val k1: Double = 0.1
  val k2: Double = 0.7

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

    val timer = new javax.swing.Timer(5, Swing.ActionListener(e =>
    {
      this.repaint
    }))
    timer.start
  }

  def vision_size(): Int = {
    var odd = (((1 to dim_x).toStream).filter(_ % 2 != 0))(vision)
    (odd * odd)
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

    this.synchronized {
    	if(data(ants(idx).x)(ants(idx).y) == null) {
      		if(ants(idx).carrying != null) drop(idx)
    	} else {
      		if(ants(idx).carrying == null) pickup(idx)
    	}	
    }
    

  }

  def density(idx: Int): Double = {
    var somatory: Double = 0.0
    for(i: Int <- -vision to vision) {
      for(j: Int <- -vision to vision) {
        var x = ants(idx).x + i
        var y = ants(idx).y + j

        if(x >= dim_x) x = x - dim_x
        if(y >= dim_y) y = y - dim_y

        if(x < 0) x = x + dim_x
        if(y < 0) y = y + dim_y

        if(!(i == 0 && j == 0)){
          this.synchronized {
            if(data(x)(y) != null ) {
              if(ants(idx).carrying != null)
                somatory += ( 1 - (distance(ants(idx).carrying.attributes, data(x)(y).attributes) / alpha ))
              else {
              	if(data(ants(idx).x)(ants(idx).y) == null) println("fumiga")
              	if(data(x)(y) == null) println("data")
              	somatory += ( 1 - (distance(data(ants(idx).x)(ants(idx).y).attributes, data(x)(y).attributes) / alpha ))
              }
                
            }
          }
        }
      }
    }
    var value: Double = 0
    value = (somatory / vision_size)

    if(value < 0)
      0.0
    else
      value
  }

  def distance(value1: Buffer[Double], value2: Buffer[Double]): Double = {
    var dist: Double = 0
    for(i: Int <- 0 until value1.length) {
      dist += Math.pow( (value1(i) - value2(i)) , 2)
    }
    Math.sqrt(dist)
  }

  def pickup(idx: Int): Unit = {
    val pp = Math.pow((k1 / (k1 + density(idx))), 2)
    val prob_random: Double = rand.nextDouble()
    
    if( prob_random < pp && data(ants(idx).x)(ants(idx).y) != null) {
    	ants(idx).carrying = data(ants(idx).x)(ants(idx).y)
    	data(ants(idx).x)(ants(idx).y) = null
    }
  }

  def drop(idx: Int): Unit = {
    val d: Double = density(idx)
    val pd = Math.pow((d / (k2 + d)), 2)
    val prob_random: Double = rand.nextDouble()
    
    if( prob_random < pd && data(ants(idx).x)(ants(idx).y) == null) {
    	data(ants(idx).x)(ants(idx).y) = ants(idx).carrying
        ants(idx).carrying = null
    }
  }

  def normalize(vectorLength: Int) = {
    for(x: Int <- 0 until vectorLength) {
      var sum: Double = 0
      var max: Double = 100000000.0
      var min: Double = -10000000.0
      for {
        i: Int <- 0 until dim_x
        j: Int <- 0 until dim_y
      } {
        if(data(i)(j) != null) {
          var value = data(i)(j).attributes(x)
          sum += value
          if (value > max) max = value
          if (value < min) min = value
        }
      }

      for {
        i: Int <- 0 until dim_x
        j: Int <- 0 until dim_y
      } {
        if(data(i)(j) != null) {
          data(i)(j).attributes(x) = (data(i)(j).attributes(x) - min)/(max - min)
        }
      }
    }
  }

  def readFile(): Unit = {
    var ncolors: Int = 0
    var nattributes = 0
    for(line <- Source.fromFile(filename).getLines) {
      var att: Buffer[Double] = Buffer()
      var x = 0
      var y = 0

      if(!line.startsWith("#")) {
        if(!line.equals("")) {
          val splitted = line.split(separator)

          for(i: Int <- 0 until splitted.length - 1) {
            if(!separator.equals(",") && splitted(i).contains(",")) splitted(i) = splitted(i).replaceAll(",", ".")
            att = att ++ Buffer((splitted(i)).toDouble)
          }

          do {
            x = rand.nextInt(dim_x)
            y = rand.nextInt(dim_y)
          } while(data(x)(y) != null)
          data(x)(y) = new Info(att, Integer.parseInt(splitted.last))
          nattributes = att.length
        }
      }
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
      println("args: dim_x dim_y n_vivas n_mortas raio_visao nome_arquivo separador_informações alpha")
    else {
      var gg = new DrawUI(Integer.parseInt(args(0)),
                        Integer.parseInt(args(1)),
                        Integer.parseInt(args(2)),
                        Integer.parseInt(args(3)),
                        Integer.parseInt(args(4)),
                        args(5),
                        args(6),
                        args(7).toDouble)
      gg.main(args)
    }
  }
}
