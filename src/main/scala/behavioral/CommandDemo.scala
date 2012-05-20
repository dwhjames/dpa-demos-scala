package behavioral

object CommandDemo {

  def main(args: Array[String]): Unit = {
    val calc = new Calc
    val user = new User
    
    user.doIt { () => calc.operation(Operator.Add, 4711) }
    user.doIt { () => calc.operation(Operator.Sub, 815) }
    user.redoit()
    user.doIt { () => calc.operation(Operator.Mul, 2) }
    user.redoit()
  }
  
}

object Operator extends Enumeration {
  type Operator = Value
  val Add, Sub, Mul, Div = Value
}
import Operator._

class Calc {
  private var result = 0
  
  def operation(operator: Operator, operand: Int) {
    operator match {
      case Add => result += operand
      case Sub => result -= operand
      case Mul => result *= operand
      case Div => result /= operand
    }
    println("result = " + result)
  }
}

class User {
  var saved: () => Unit = _
  
  def doIt(f: () => Unit) {
    saved = f
    f()
  }
  
  def redoit() {
    saved()
  }
}
