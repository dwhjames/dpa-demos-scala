package behavioral

object TemplateMethodDemo {

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 10) printf("Collatz of %d took %d steps\n", i, Collatz.run(i))
  }
}

abstract class Template {
  def validInput(i: Int): Boolean
  def loopPredicate(i: Int): Boolean
  def stepPredicate(i: Int): Boolean
  def stepThen(i: Int): Int
  def stepElse(i: Int): Int
  
  final def run(i: Int): Int = {
    var n = i
    var steps = -1
    if (validInput(n)) {
      steps += 1
      while (loopPredicate(n)) {
        if (stepPredicate(n)) {
          n = stepThen(n)
        } else {
          n = stepElse(n)
        }
        steps += 1
      }
    }
    steps
  }
}

object Collatz extends Template {
  override def validInput(i: Int) = i > 0
  override def loopPredicate(i: Int) = i != 1
  override def stepPredicate(i: Int) = (i % 2) == 0
  override def stepThen(i: Int) = i / 2
  override def stepElse(i: Int) = 3*i + 1
}
