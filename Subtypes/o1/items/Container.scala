package o1.items
import scala.collection.mutable.Buffer

// TODO: complete as instructed in Chapter 7.3.

class Container(name:String) extends Item(name) {

  private val content = Buffer[Item]()

  def addContent(newContent: Item): Unit = {
    content += newContent
  }

  override def toString = s"$name containing " + content.size + " item(s)"



}