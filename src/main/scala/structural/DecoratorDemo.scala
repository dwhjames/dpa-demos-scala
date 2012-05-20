package structural

object DecoratorDemo {

  def main(args: Array[String]): Unit = {
    var drink: Drink = new Coffee
  	println(drink)
		
  	drink = new Milk(drink)
  	println(drink)
	
  	drink = new Decaffe(drink)
  	println(drink)
	
  	drink = new Ice(drink)
  	println(drink)
  }
}

trait Drink {
  val name: String
  def ingredients: List[String]
  def hot: Boolean
  def caffeinated: Boolean
  def cost: Double
  override def toString(): String = {
    val s = new StringBuilder
    s.append(if (hot) "A hot" else "An iced")
    if (!caffeinated) s.append(", decaffe")
  	s.append(' ').append(name)
  	ingredients foreach { s.append(" with ").append(_) }
  	s.append(" for $").append("%1.2f" format cost).toString()
  }
}

class Coffee extends Drink {
  val name = "coffee"
  override def ingredients = Nil
  override def hot = true
  override def caffeinated = true
  override def cost = 1.0
}

abstract class DrinkDecorator(drink: Drink) extends Drink {
  val name = drink.name
  override def ingredients = drink.ingredients
  override def hot = drink.hot
  override def caffeinated = drink.caffeinated
  override def cost = drink.cost
}

class Milk(drink: Drink) extends DrinkDecorator(drink) {
  override def ingredients = "milk" :: drink.ingredients
  override def cost = drink.cost + 0.1
}

class Ice(drink: Drink) extends DrinkDecorator(drink) {
  override def hot = false
  override def cost = drink.cost + 0.2
}

class Decaffe(drink: Drink) extends DrinkDecorator(drink) {
  override def caffeinated = false
  override def cost = drink.cost - 0.2
}
