package structural

import scala.util.Random

object FacadeDemo {

  def main(args: Array[String]): Unit = {
    val cc = new CreditCheckFacade
    val customers = Seq(Customer("Ann"), Customer("Bob"), Customer("Charlie"))
    
    for (c <- customers) {
      printf("%s has a credit score of %d\n\n", c.name, cc.computeCreditScore(c))
    }
  }
}

case class Customer(val name: String)

class Bank {
  val r = new Random
  def hasExtendedOverdraftFor(c: Customer): Boolean = {
    println("Checking presence of overdraft for " + c.name)
    r.nextBoolean()
  }
}

class CreditCardIssuer {
  val r = new Random
  def checkMissedPaymentsFor(c: Customer): Boolean = {
    printf("Checking if %s has missed credit card payments\n", c.name)
    r.nextBoolean()
  }
}

class LoanCompany {
  val r = new Random
  def checkDefaultFor(c: Customer): Boolean = {
    printf("Checking if %s has defaulted on any loans\n", c.name)
    r.nextBoolean()
  }
}

class CreditCheckFacade {
  val b  = new Bank
  val i = new CreditCardIssuer
  val l  = new LoanCompany
  
  def computeCreditScore(c: Customer): Int = {
    println("Computing credit score for " + c.name)
    var score = 100
    if (b hasExtendedOverdraftFor c) score -= 10
    if (i checkMissedPaymentsFor c)  score -= 15
    if (l checkDefaultFor c)         score -= 75
    score
  }
}
