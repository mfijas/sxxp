package org.sxxp.xpath1.parser.axis

class NodePath(val path: Vector[Int]) extends Ordered[NodePath] {
  def append(childIndex: Int) = new NodePath(path :+ childIndex)

  def parentPath = new NodePath(path dropRight 1)

  override def compare(that: NodePath) = {
    var i = 0
    while (i < this.path.length && i < that.path.length && path(i) == that.path(i))
      i += 1

    if (i == that.path.length && i == this.path.length)
      0
    else if (i == this.path.length)
      -1
    else if (i == that.path.length)
      1
    else
      this.path(i).compareTo(that.path(i))
  }

  override def equals(other: Any): Boolean = other match {
    case that: NodePath => path == that.path
    case _ => false
  }

  override def hashCode = path.hashCode()

  override def toString = s"NodePath(${path.mkString(", ")})"
}

object NodePath {
  val empty = NodePath()

  def apply(elem: Int*) = new NodePath(Vector(elem: _*))
}
