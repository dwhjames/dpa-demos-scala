package structural

import scala.collection.mutable.HashMap
import scala.ref.WeakReference
import scala.ref.ReferenceQueue

object FlyweightDemo {

  def main(args: Array[String]): Unit = {
    var times = Font("Times")
    var times2 = Font("Times")
    println(times eq times2)
    times = null
    times2 = null
    System.gc()
    System.gc()
    System.gc()
    System.gc()
    System.gc()
    Font.groom()
  }
}

trait FlyweightFactory {
  type Key
  type Flyweight <: AnyRef
  
  def create(key: Key): Flyweight
  
  val flyweightPool = new HashMap[Key, WeakReference[Flyweight]]
  val refQueue = new ReferenceQueue[Flyweight]
  
  def apply(key: Key): Flyweight = {
    def add(key: Key): Flyweight = {
      val fly = create(key)
      if (fly != null) {
        flyweightPool.update(key, new WeakReference(fly, refQueue))
      }
      fly
    }
    flyweightPool.get(key) match {
      case Some(ref) =>
        ref.get.getOrElse(add(key))
      case None => add(key)
    }
  }
  
  def groom() {
    var ref = refQueue.poll
    var nonEmpty = false
    while (ref.isDefined) {
      println("found reference in queue")
      nonEmpty = true
      ref = refQueue.poll
    }
    if (nonEmpty) {
    	flyweightPool.retain {
    	  (k, ref) =>
    	    if (ref.get.isEmpty)
    	      println("memory for " + k + " font was reclaimed")
    	    ref.get.isDefined
    	}
    }
  }
}

class Font private(name: String) {
  override def toString = "Font name = " + name
}

object Font extends FlyweightFactory {
  type Key = String
  type Flyweight = Font
  
  override def create(name: String) = {
    println("Loading font " + name)
    new Font(name)
  }
}
