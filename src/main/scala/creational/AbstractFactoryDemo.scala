package creational

object AbstractFactoryDemo {

  def main(args: Array[String]): Unit = {
    println("Concrete Factories")
    val factories: Array[AbstractFactory] = Array(new ConcreteFactory1, new ConcreteFactory2)
    for (factory <- factories) {
      println(factory)
      println(factory.createProductA())
      println(factory.createProductB())
    }
    println()
    
    println("Parameterized Factories")
    ProductClass.values foreach { productClass =>
      val factory = new ParamFactory(productClass)
      println(factory)
      println(factory.createProductA())
      println(factory.createProductB())
    }
    println()
    
    println("Generic Factories")
    val genericFactories: Array[AbstractFactory] = Array(new GenericFactory1, new GenericFactory2)
    for (factory <- genericFactories) {
      println(factory)
      println(factory.createProductA())
      println(factory.createProductB())
    }
  }
}

trait AbstractProductA
class ProductA1 extends AbstractProductA
class ProductA2 extends AbstractProductA

trait AbstractProductB
class ProductB1 extends AbstractProductB
class ProductB2 extends AbstractProductB

abstract class AbstractFactory {
  def createProductA(): AbstractProductA
  def createProductB(): AbstractProductB
}

class ConcreteFactory1 extends AbstractFactory {
  override def createProductA(): AbstractProductA = new ProductA1
  override def createProductB(): AbstractProductB = new ProductB1
}

class ConcreteFactory2 extends AbstractFactory {
  override def createProductA(): AbstractProductA = new ProductA2
  override def createProductB(): AbstractProductB = new ProductB2
}


object ProductClass extends Enumeration {
  val One, Two = Value
}

class ParamFactory(val productClass: ProductClass.Value) extends AbstractFactory {
  override def createProductA(): AbstractProductA = productClass match {
    case ProductClass.One => new ProductA1
    case ProductClass.Two => new ProductA2
  }
  override def createProductB(): AbstractProductB = productClass match {
    case ProductClass.One => new ProductB1
    case ProductClass.Two => new ProductB2
  }
  override def toString() =
    this.getClass().getName() +
      "(" + productClass + ")" +
      "@" + Integer.toHexString(this.hashCode())
}

abstract class AbstractGenericFactory extends AbstractFactory {
  val productAType: Class[_ <: AbstractProductA]
  val productBType: Class[_ <: AbstractProductB]
  
  override def createProductA(): AbstractProductA = productAType.newInstance()
  override def createProductB(): AbstractProductB = productBType.newInstance()
}

class GenericFactory1 extends AbstractGenericFactory {
  val productAType = classOf[ProductA1]
  val productBType = classOf[ProductB1]
}

class GenericFactory2 extends AbstractGenericFactory {
  val productAType = classOf[ProductA2]
  val productBType = classOf[ProductB2]
}
