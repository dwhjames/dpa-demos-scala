package behavioral

import scala.collection.mutable.ListBuffer

object VisitorDemo {

  def main(args: Array[String]): Unit = {
    val p1 = new Paragraph("Object-oriented Design")
    val p2 = new Paragraph("Design Patterns")
    val s = new Section("Courses")
    s.add(p1, p2)
    
    val c = new Section("Programme")
    c.add(s)
    
    println("*Display document with GoF Visitor pattern*")
    c.accept(new Display)
    
    println()
    
    println("*Display document with pattern matching*")
    Display.display(c)
  }
}

trait Document {
  def contains(s: String): Boolean
  def accept(v: Visitor): Unit
}

trait Visitor {
  def visit(p: Paragraph): Unit
  def visit(s: Section): Unit
}

class Paragraph(val paragraph: String) extends Document {
  override def contains(s: String) = paragraph.contains(s)
  override def accept(v: Visitor) = v.visit(this)
}

object Paragraph {
  def unapply(p: Paragraph): Option[String] = Some(p.paragraph)
}

class Section(val title: String) extends Document {
  private val children = new ListBuffer[Document]
  
  def add(docs: Document*): Section = { children.append(docs:_*); this }
  
  override def contains(s: String): Boolean = {
    for (child <- children if child.contains(s)) return true
    return false
  }
  
  override def accept(v: Visitor) = {
    v.visit(this)
    children.foreach(_.accept(v))
  }
}

object Section {
  def unapply(s: Section): Option[(String, Seq[Document])] = Some(s.title, s.children)
}

class Display extends Visitor {
  override def visit(p: Paragraph) = println(p.paragraph)
  
  override def visit(s: Section) = {
    println(s.title)
    for (i <- 0 until s.title.length) print('=')
    println()
  }
}

object Display {
  def display(d: Document): Unit = d match {
    case Paragraph(para) => println(para)
    case Section(title, children) => {
      println(title)
      for (i <- 0 until title.length) print('=')
      println()
      children.foreach(display(_))
    }
  }
}
