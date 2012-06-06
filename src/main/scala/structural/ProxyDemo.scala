package structural

object ProxyDemo {

  def main(args: Array[String]) {
    println("Demo for Virtual Proxy")
    println("----------------------")
    virtual.VirtualDemo.demo()
    println()
    println("Demo for Remote Proxy")
    println("---------------------")
    remote.RemoteDemo.demo()
  }
}

package virtual {

  object VirtualDemo {
    def demo() {
      val img1: Image = new ImageProxy("image1.jpg")
      val img2: Image = new HighResImage("image2.jpg")
      val img3: Image = new ImageProxy("image3.jpg")
      val img4: Image = new HighResImage("image4.jpg")

      img3.showImage()

      img4.showImage()
    }
  }

  trait Image {
    def showImage(): Unit
  }

  class HighResImage(val path: String) extends Image {
    val image = loadImage(path)

    def loadImage(path: String) {
      println("Loading expensive image from path: " + path)
      Thread.sleep(1000)
      println("Loaded expensive image from path: " + path)
    }

    override def showImage() {
      println("Showing image from path: " + path)
    }
  }

  class ImageProxy(val path: String) extends Image {
    var proxy: Image = _

    override def showImage() {
      proxy = new HighResImage(path)
      proxy.showImage()
    }
  }
}

package remote {
  import java.io.{ObjectInputStream,ObjectOutputStream}
  import java.net.{InetAddress,ServerSocket,Socket}
  import RichSocket._

  object RemoteDemo {
    def demo() {
      new Server().start()
      val reducer: Reducer = new ReducerProxy
      val result = reducer.reduce(1,2,3)
      println("Reducer returned: " + result)
    }
  }

  trait Reducer {
    def reduce(s: Int*): Int
  }

  class ReducerImpl extends Reducer {
    override def reduce(s: Int*): Int = s.reduce(_ + _)
  }

  class ReducerProxy extends Reducer {
    override def reduce(s: Int*): Int = {
      var result: Int = 0
      try {
        val ia = InetAddress.getByName("localhost")
        val socket = new Socket(ia, 54321)
        println("Client opened a connection to port 54321 at localhost")
        try {
          socket.withObjectStreams { (in, out) =>
            out.writeObject(s.toSeq)
            println("Client wrote a Seq of Ints")
            result = in.readInt()
            println("Client read an Int")
          }
        } finally {
          socket.close()
          println("Client closed the connection")
        }
      } catch {
        case ex: Exception =>
          System.err.println(ex.getMessage())
          System.exit(-1)
      }
      result
    }
  }

  class Server extends Thread("Server") {
    override def run() {
      try {
        println("Server started")
        val listener = new ServerSocket(54321)
        println("Server opened port 54321")
        val socket = listener.accept()
        println("Server accepted a connection")
        try {
          socket.withObjectStreams { (in, out) =>
            val seq  = in.readObject().asInstanceOf[Seq[Int]]
            println("Server read a Seq of Ints")
            val reducer = new ReducerImpl
            out.writeInt(reducer.reduce(seq:_*))
            println("Server wrote an Int")
          }
        } finally {
          socket.close()
          println("Server closed the connection")
          listener.close()
          println("Server closed port")
        }
      } catch {
        case ex: Exception =>
          System.err.println(ex.getMessage())
          System.exit(-1)
      }
    }
  }

  class RichSocket(socket: Socket) {
    def withObjectStreams(f: (ObjectInputStream, ObjectOutputStream) => Unit) {
      val out = new ObjectOutputStream(socket.getOutputStream())
      val in  = new ObjectInputStream(socket.getInputStream())
      try {
        f(in,out)
      } finally {
        out.flush()
        out.close()
        in.close()
      }
    }
  }
  object RichSocket {
    implicit def enrichSocket(s: Socket): RichSocket = new RichSocket(s)
  }
}
