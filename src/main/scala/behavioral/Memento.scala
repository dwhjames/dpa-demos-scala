package behavioral

object MementoDemo {

  def main(args: Array[String]): Unit = {
    val v = new Vector
    val iter = v.iterator
    val snap = iter.createSnapshot
    for (i <- iter) print(i)
    println(" - initial iteration")
    for (i <- iter) print(i)
    println(" - iteration with expended iterator")
    iter.restoreSnapshot(snap)
    for (i <- iter) print(i)
    println(" - iteration with reset iterator")
  }
}

trait SnapshotIterator[+A] extends Iterator[A] {
  def createSnapshot: IteratorState
  def restoreSnapshot(snap: IteratorState): Unit
}

trait IteratorState {
  def getState: Int
}


class Vector {
  private val arr: Array[Int] = 0 until 10 toArray

  def iterator = new SnapshotIterator[Int] {
    var ix = 0

    override def createSnapshot = new IteratorState {
      val getState = ix
    }

    override def restoreSnapshot(state: IteratorState) =
      ix = state.getState

    override def hasNext = ix < arr.length

    override def next = {
      val a = arr(ix)
      ix += 1
      a
    }
  }
}
