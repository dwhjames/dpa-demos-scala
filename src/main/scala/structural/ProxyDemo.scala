package structural

object ProxyDemo {

  def main(args: Array[String]) {
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
