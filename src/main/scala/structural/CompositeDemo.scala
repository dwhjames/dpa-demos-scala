package structural

import scala.collection.mutable.ListBuffer

object CompositeDemo {

  def main(args: Array[String]): Unit = {
    val p1 = new Paragraph("Object-oriented Design")
    val p2 = new Paragraph("Design Patterns")
    val s = new Section("Courses")
    s.add(p1, p2)
    
    val c = new Section("Programme")
    c.add(s)
    
    println(c.title + " contains \"Design\": " + c.contains("Design"))
    println(c.title + " does not contain \"Scala\": " + !c.contains("Scala"))
  }
}

trait Document {
  def contains(s: String): Boolean
}

class Paragraph(val paragraph: String) extends Document {
  override def contains(s: String) = paragraph.contains(s)
}

class Section(val title: String) extends Document {
  val children = new ListBuffer[Document]
  
  def add(docs: Document*): Section = { children.append(docs:_*); this }
  override def contains(s: String): Boolean = {
    for (child <- children if child.contains(s)) return true
    return false
  }
}
