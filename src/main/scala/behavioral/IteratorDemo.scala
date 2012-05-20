package behavioral

import scala.collection.mutable.Stack

object IteratorDemo {

  def main(args: Array[String]): Unit = {
    val root = Branch(Branch(Branch(Leaf,
                                    1,
                                    Leaf),
                             2,
                             Branch(Leaf,
                                    3,
                                    Leaf)),
                      4,
                      Branch(Branch(Leaf,
                                    5,
                                    Leaf),
                             6,
                             Branch(Leaf,
                                    7,
                                    Leaf)))
    
    for (i <- Tree.inOrder(root)) println(i)
    println()
    for (i <- Tree.preOrder(root)) println(i)
    println()
    for (i <- Tree.postOrder(root)) println(i)
  }
}

sealed abstract class Tree[+E]
object Leaf extends Tree[Nothing]
case class Branch[E](left: Tree[E], label: E, right: Tree[E]) extends Tree[E]

object Tree {
  def inOrder[E](root: Tree[E]): Iterator[E] = new Iterator[E] {
    val stack = new Stack[Branch[E]]
    spine(root)
    
    def spine(t: Tree[E]): Unit = t match {
      case b@Branch(left,_,_) => {
        stack.push(b)
        spine(left)
      }
      case _ => ()
    }
    
    def hasNext: Boolean = !stack.isEmpty
    
    def next(): E = {
      stack.pop() match {
        case Branch(_, label, Leaf) => label
        case Branch(_, label, right) => {
          spine(right)
          label
        }
      }
    }
  }
  
  def preOrder[E](root: Tree[E]): Iterator[E] = new Iterator[E] {
    val stack = new Stack[Branch[E]]
    pushIfBranch(root)
    
    def pushIfBranch(t: Tree[E]): Unit = t match {
      case b@Branch(_,_,_) => stack.push(b)
      case _ => ()
    }
    
    def hasNext: Boolean = !stack.isEmpty
    
    def next(): E = {
      stack.pop() match {
        case Branch(left, label, right) => {
          pushIfBranch(right)
          pushIfBranch(left)
          label
        }
      }
    }
  }
  
  def postOrder[E](root: Tree[E]): Iterator[E] = new Iterator[E] {
    val stack = new Stack[Branch[E]]
    spine(root)
    
    def spine(t: Tree[E]): Unit = t match {
      case b@Branch(Leaf,_,right) => {
        stack.push(b)
        spine(right)
      }
      case b@Branch(left,_,_) => {
        stack.push(b)
        spine(left)
      }
      case _ => ()
    }
    
    def hasNext: Boolean = !stack.isEmpty
    
    def next(): E = {
      stack.pop() match {
        case b@Branch(_,label,_) => {
          if (!stack.isEmpty && !stack.top.right.eq(b)) {
            spine(stack.top.right)
          }
          label
        }
      }
    }
  }
}
