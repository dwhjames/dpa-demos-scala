package behavioral

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.StringBuilder

object VisitorDemo {

  def main(args: Array[String]): Unit = {
    val p1 = new Paragraph("Object-oriented Design")
    val p2 = new Paragraph("Design Patterns")
    val s = new Section("Courses")
    s.add(p1, p2)
    
    val c = new Section("Programme")
    c.add(s)
    
    println("*Display document with Internal Visitor pattern*")
    print(c.acceptInternal(new Display))

    println("*Display document with pattern matching*")
    Display.display(c)

    println("*Check membership with External Visitor pattern*")
    println(c.title + " contains \"Design\": " + c.acceptExternal(new Contains("Design")))
    println(c.title + " does not contain \"Scala\": " + !c.acceptExternal(new Contains("Scala")))

    println("*Check membership with direct recursion*")
    println(c.title + " contains \"Design\": " + c.contains("Design"))
    println(c.title + " does not contain \"Scala\": " + !c.contains("Scala"))
  }
}

trait Document {
  def contains(s: String): Boolean
  def acceptInternal[R](v: InternalVisitor[R]): R
  def acceptExternal[R](v: ExternalVisitor[R]): R
}

trait InternalVisitor[R] {
  def visitParagraph(paragraph: String): R
  def visitSection(title: String, children:List[R]): R
}

trait ExternalVisitor[R] {
  def visitParagraph(paragraph: String): R
  def visitSection(title: String, children:List[Document]): R
}

class Paragraph(val paragraph: String) extends Document {
  override def contains(s: String) = paragraph.contains(s)
  override def acceptInternal[R](v: InternalVisitor[R]) = v.visitParagraph(paragraph)
  override def acceptExternal[R](v: ExternalVisitor[R]) = v.visitParagraph(paragraph)
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
  
  override def acceptInternal[R](v: InternalVisitor[R]) =
    v.visitSection(title, children.map(_.acceptInternal(v)).toList)

  override def acceptExternal[R](v: ExternalVisitor[R]) =
    v.visitSection(title, children.toList)
}

object Section {
  def unapply(s: Section): Option[(String, Seq[Document])] = Some(s.title, s.children)
}

class Display extends InternalVisitor[StringBuilder] {
  override def visitParagraph(paragraph: String) =
    new StringBuilder(paragraph)
  
  override def visitSection(title: String, children: List[StringBuilder]) = {
    val builder = new StringBuilder(title).append('\n')
    for (i <- 0 until title.length) builder.append('=')
    builder.append('\n')
    for (child <- children) builder.append(child).append('\n')
    builder
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

class Contains(str: String) extends ExternalVisitor[Boolean] {
  override def visitParagraph(paragraph: String) =
    paragraph.contains(str)

  override def visitSection(title: String, children: List[Document]): Boolean = {
    for (child <- children if child.acceptExternal(this)) return true
    return false
  }
}
