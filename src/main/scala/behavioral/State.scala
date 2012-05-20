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
