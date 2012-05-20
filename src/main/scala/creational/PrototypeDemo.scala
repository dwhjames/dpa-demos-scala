package creational

object PrototypeDemo {

  def main(args: Array[String]): Unit = {
  	println("PartA prototype: " + PartA.prototype)
  	println("PartB prototype: " + PartB.prototype)
  	println("Whole prototype: " + Whole.prototype)
  	assert(Whole.prototype.partA eq PartA.prototype)
  	assert(Whole.prototype.partB eq PartB.prototype)
		
  	val w: Whole = Whole.prototype.copy()
  	println("Shallow copy of Whole.prototype: " + w)
  	assert(w.partA eq PartA.prototype)
  	assert(w.partB eq PartB.prototype)
  	
  	w.partA = PartA.prototype.copy()
  	println("and with a new partA: " + w)
  	assert(w.partB eq PartB.prototype)
	
  	val w2: Whole = Whole.prototype.deepCopy()
  	println("Deep copy of Whole.prototype: " + w2)
  }
}

trait Copyable[+A <: AnyRef] extends AnyRef {
  def copy(): A
  def deepCopy(): A
}


trait Whole extends Copyable[Whole] {
  var partA: PartA
  var partB: PartB
}

trait PartA extends Copyable[PartA]

trait PartB extends Copyable[PartB]

object Whole {
  val prototype: Whole = new WholeImpl()
  
  private class WholeImpl extends Whole {
    var partA = PartA.prototype
    var partB = PartB.prototype
    
    override def copy(): Whole = {
      val whole: Whole = new WholeImpl()
      whole.partA = this.partA
      whole.partB = this.partB
      return whole
    }
    
    override def deepCopy: Whole = {
      val whole: Whole = new WholeImpl()
      whole.partA = this.partA.deepCopy()
      whole.partB = this.partB.deepCopy()
      return whole
    }
    
    override def toString() =
      "Whole@" + Integer.toHexString(this.hashCode()) + "(" + partA.toString() + ", " + partB.toString() + ")"
  }
}

object PartA {
  val prototype: PartA = new PartAImpl()
  
  private class PartAImpl extends PartA {
    override def copy(): PartA = new PartAImpl()
    override def deepCopy() = copy()
    override def toString() = "PartA@" + Integer.toHexString(this.hashCode())
  }
}

object PartB {
  val prototype: PartB = new PartBImpl()
  
  private class PartBImpl extends PartB {
    override def copy(): PartB = new PartBImpl()
    override def deepCopy() = copy()
    override def toString() = "PartB@" + Integer.toHexString(this.hashCode())
  }
}
