package behavioral

object ChainOfResponsibilityDemo {

  def main(args: Array[String]): Unit = {
    import withclasses._
    val chain = new Manager(new Director)
    chain.approve(25)
    chain.approve(75)
    chain.approve(125)
    
    import withpartialfunc._
    Demo.chain(25)
    Demo.chain(75)
    Demo.chain(125)
  }
}

package withclasses {
  trait Approver[-Req] {
    def approve(request: Req): Unit
  }
  
  class NullApprover extends Approver[Any] {
    override def approve(request: Any) {
      println("NullApprover approved nothing")
    }
  }
  
  class Manager(next: Approver[Int]) extends Approver[Int] {
    def this() = this(new NullApprover)
    
    override def approve(request: Int) {
      if (request <= 50)
        println("The manager approved a request for $" + request)
      else
        next.approve(request)
    }
  }
  
  class Director(next: Approver[Int]) extends Approver[Int] {
    def this() = this(new NullApprover)
    
    override def approve(request: Int) {
      if (request <= 100)
        println("The director approved a request for $" + request)
      else
        next.approve(request)
    }
  }
}

package withpartialfunc {
  object Demo {
    type Approver = PartialFunction[Int, Unit]
    
    val nullApprover: Approver = { case _ => println("NullApprover approved nothing") }
    val manager: Approver = { case i if i <= 50 => println("The manager approved a request for $" + i) }
    val director: Approver = { case i if i <= 100 => println("The director approved a request for $" + i) }
    
    val chain = manager orElse director orElse nullApprover
  }
}
