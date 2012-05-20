package structural

import scala.math

object AdapterDemo {

  def main(args: Array[String]): Unit = {
    println("Adapter via object composition")
    composition.CompositionDemo.demo()
    println()
    println("Adapter via inheritance")
    inheritance.InheritanceDemo.demo()
  }
}

package composition {
  class SquarePeg(val width: Int)
  
  class RoundPeg(val radius: Double)
  
  class RoundHole(val radius: Double) {
    def pegFits(peg: RoundPeg) = peg.radius <= radius
    override def toString() = "RoundHole with radius " + radius
  }
  
  class SquarePegAdapter(peg: SquarePeg)
    extends RoundPeg(math.sqrt(2*(math.pow(peg.width / 2.0, 2)))) {
    override def toString() =
      "SquarePegAdapter with peg width " + peg.width +
        " (and notional radius " + radius + ")"
  }
  
  object CompositionDemo {
    def demo() {
      val hole = new RoundHole(4.0)
      for (w <- 4 to 7) {
        val peg = new SquarePegAdapter(new SquarePeg(w))
        if (hole.pegFits(peg))
          println("peg " + peg + " fits in hole " + hole)
        else
          println("peg " + peg + " does not fit in hole " + hole)
      }
    }
  }
}

package inheritance {
  class SquarePeg(val width: Int)
  
  trait RoundPeg {
    val radius: Double
  }
  
  class RoundPegImpl(val radius: Double) extends RoundPeg
  
  class RoundHole(val radius: Double) {
    def pegFits(peg: RoundPeg) = peg.radius <= radius
    override def toString() = "RoundHole with radius " + radius
  }
  
  class SquarePegAdapter(width: Int) extends SquarePeg(width) with RoundPeg {
    val radius = math.sqrt(2*(math.pow(width / 2.0, 2)))
    override def toString() =
      "SquarePegAdapter with peg width " + width +
        " (and notional radius " + radius + ")"
  }
  
  object InheritanceDemo {
    def demo() {
      val hole = new RoundHole(4.0)
      for (w <- 4 to 7) {
        val peg = new SquarePegAdapter(w)
        if (hole.pegFits(peg))
          println("peg " + peg + " fits in hole " + hole)
        else
          println("peg " + peg + " does not fit in hole " + hole)
      }
    }
  }
}
