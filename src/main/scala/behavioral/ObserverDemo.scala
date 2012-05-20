package behavioral

import scala.collection.Set
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.WeakHashMap

object ObserverDemo {

  def main(args: Array[String]): Unit = {
    react.ReactObserverDemo.demo()
  }
}

package java {
  
  trait Subscriber[-Evt, -Pub] {
    def notify(pub: Pub, event: Evt): Unit
  }
  
  trait Publisher[Evt] {
    type Pub <: Publisher[Evt]
    type Sub = Subscriber[Evt, Pub]
    
    private val subs = new HashSet[Sub]
    
    def subscribe(sub: Sub) { subs += sub }
    def unsubscribe(sub: Sub) { subs -= sub }
    
    protected def publish(event: Evt) {
      subs foreach { _.notify(this.asInstanceOf[Pub], event) }
    }
  }
}

package csharp {
  
  trait Disposable {
    def dispose()
  }
  
  trait Observer[-T] {
    def onCompleted()
    def onError(error: Exception)
    def onNext(value: T)
  }
  
  trait Observable[+T] {
    def subscribe(observer: Observer[T]): Disposable
  }
}

package react {
  
  trait Observer {
    def receive()
  }
  
  trait Events[Msg] {
    private var obs = new WeakHashMap[Observer,Unit]
    private var cachedMsg = None: Option[Msg]
    
    def subscribe(ob: Observer) { println("subscribe ob"); obs += ((ob, ())) }
    
    private def clearObservers(): Set[Observer] = {
      val old = obs.keySet
      println("Clearing Observers")
      println("Old: " + old)
      obs = new WeakHashMap[Observer,Unit]
      old
    }
    
    def message(): Option[Msg] = cachedMsg
    
    protected def propogate(msg: Msg) {
      cachedMsg = Some(msg)
      clearObservers() foreach (_.receive())
      cachedMsg = None
    }
  }
  
  class EventSource[Msg] extends Events[Msg] {
    println("Creating EventSource")
    def emit(msg: Msg) = {
      println("Emitting: " + msg)
      propogate(msg)
    }
  }
  
  trait Observing {
    private val obRefs = new ListBuffer[Observer]
    
    private def ref(ob: Observer) { println("ref ob"); obRefs += ob }
    private def unref(ob: Observer) { println("unref ob"); obRefs -= ob }
    
    protected abstract class PersistantObserver extends Observer {
      ref(this)
      def dispose() { unref(this) }
    }
    
    protected def observe[A](e: Events[A])(f: A => Boolean) = {
      val ob = new PersistantObserver {
        def receive() {
          println("Observer's receive triggered")
          e.message match {
            case Some(msg) => {
              println("Observer retreived message: " + msg)
              if (f(msg)) e.subscribe(this) else this.dispose()
            }
            case _ => e.subscribe(this)
          }
        }
      }
      e.subscribe(ob)
      ob
    }
  }
  
  object ReactObserverDemo extends Observing {
    def demo() {
      val es = new react.EventSource[Int]
      es emit 1
      val ob = observe(es) { x => println(x); x < 3 }
      es emit 2
      es emit 3
      es emit 4
    }
  }
}
