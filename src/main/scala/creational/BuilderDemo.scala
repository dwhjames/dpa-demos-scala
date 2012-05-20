package creational

import scala.collection.mutable.ListBuffer

object BuilderDemo {

  def main(args: Array[String]): Unit = {
    val b = new DocumentBuilder("Programme")
                      .addSection("Courses")
                      .addParagraph("Object-oriented Design",
                                    "Design Patterns")
    val d = b.doc
    println(d.title + " contains \"Design\": " + d.contains("Design"))
    println(d.title + " does not contain \"Scala\": " + !d.contains("Scala"))
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

class DocumentBuilder(val doc: Section) {
  
  def this(title: String) = this(new Section(title))
  
  def addParagraph(paragraph: Paragraph): DocumentBuilder = { doc.add(paragraph); this }
  
  def addParagraph(paragraphs: String*): DocumentBuilder = {
    paragraphs foreach { s => addParagraph(new Paragraph(s)) }
    this
  }
  
  def addSection(section: Section): DocumentBuilder = {
    doc.add(section)
    new DocumentBuilder(section)
  }
  
  def addSection(title: String): DocumentBuilder = addSection(new Section(title))
}
