package creational

import scala.math

object FactoryMethodDemo {

  def main(args: Array[String]): Unit = {
  	val cart = Complex.fromCartesian(3.0, 4.0);
      printf("real: %.3f, img: %.3f, abs: %.3f, arg: %.3f\n",
             cart.real,
             cart.img,
             cart.abs,
             cart.arg)
		
  	val polar = Complex.fromPolar(5, math.Pi / 6.0);
  	printf("real: %.3f, img: %.3f, abs: %.3f, arg: %.3f\n",
  	       polar.real,
  		     polar.img,
  		     polar.abs,
  		     polar.arg)
  }
}

class Complex private(r: Double, i: Double) {
  def real = r
  def img = i
  def abs = math.sqrt(r*r + i*i)
  def arg = math.atan2(i, r)
}

object Complex {
  def fromCartesian(real: Double, imaginary: Double): Complex =
    new Complex(real,imaginary)
  
  def fromPolar(absolute: Double, argument: Double): Complex =
    new Complex(absolute * math.cos(argument), absolute * math.sin(argument))
}
