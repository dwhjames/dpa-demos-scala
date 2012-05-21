package behavioral

object StateDemo {
  def main(args: Array[String]) {
    val c = new Connection
    c send "foo"
    c.connect()
    c send "bar"
    c.connect()
    c send "baz"
    c.disconnect()
    
    println()
    
    dsl.DSLDemo.demo()
  }
}

class Connection {
  import Connection.{State,Offline}
  private var state: State = Offline
  
  def send(s: String) = state.send(this, s)
  def connect() = state connect this
  def disconnect() = state disconnect this
  
  private def changeState(s: State) {
    println(state + " -> " + s)
    state = s
  }
  
  private def internalSend(s: String) {
    println("Sent: " + s)
  }
  
  private def stateError(s: State) {
    println("Error in " + s + " state")
  }
}

object Connection {
  private abstract class State {
    def send(c: Connection, s: String) = c stateError this
    def connect(c: Connection) = c stateError this
    def disconnect(c: Connection) = c stateError this
    protected def changeState(c: Connection, s: State) = c.changeState(s)
  }

  private object Offline extends State {
    override def connect(c: Connection) = changeState(c, Online)
    override def toString() = "Offline"
  }

  private object Online extends State {
    override def send(c: Connection, s: String) = c.internalSend(s)
    override def disconnect(c: Connection) = changeState(c, Offline)
    override def toString() = "Online"
  }
}

package dsl {
  object DSLDemo {
    def demo() {
      val fsm = new FiniteStateMachine[String,String]("offline")
      fsm.record.onEvent("connect").inState("offline").moveToState("online")
      fsm.record.onEvent("send").stayInState("online")
      fsm.record.inState("online").onEvent("disconnect").moveToState("offline")
      
      fsm.fire("send")
      fsm.fire("connect")
      fsm.fire("send")
      fsm.fire("connect")
      fsm.fire("disconnect")
    }
  }

  class TransitionBuilder[S,E](private val fsm: FiniteStateMachine[S,E]) {
    private[dsl] var fromState: S = _
    private[dsl] var transEvent: E = _
    private[dsl] var toState: S = _

    def inState(state: S) = {
      fromState = state
      this
    }

    def onEvent(event: E) = {
      transEvent = event
      this
    }

    def moveToState(state: S) {
      toState = state
      fsm register this
    }

    def stayInState(state: S) {
      fromState = state
      toState = state
      fsm register this
    }
  }

  class FiniteStateMachine[S,E](val initialState: S) {
    import scala.collection.mutable.HashMap
    val machine: HashMap[S,HashMap[E,S]] = new HashMap
    var currentState = initialState

    def reset() {
      currentState = initialState
    }

    def record = new TransitionBuilder(this)

    private def stateError(s: S) {
      println("no transitions defined for state: " + s)
    }

    private def transError(e: E) {
      println("no '" + e + "' transition defined for state: " + currentState)
    }

    def fire(event: E) {
      machine.get(currentState) match {
        case None =>
          stateError(currentState)
        case Some(map) =>
          map.get(event) match {
            case None =>
              transError(event)
            case Some(state) =>
              println(event + ": " + currentState + " -> " + state)
              currentState = state
          }
      }
    }

    private[dsl] def register(b: TransitionBuilder[S,E]) {
      machine.get(b.fromState) match {
        case None =>
          machine(b.fromState) = HashMap(b.transEvent -> b.toState)
        case Some(map) =>
          map += (b.transEvent -> b.toState)
      }
    }
  }
}
