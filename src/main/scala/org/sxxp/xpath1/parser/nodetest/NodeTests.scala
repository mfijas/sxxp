package org.sxxp.xpath1.parser.nodetest

import org.sxxp.xpath1.parser.QName
import scala.xml.Node

trait NodeTest {
  def apply(node: Node): Boolean = ???
}

case class NameNodeTest(name: QName) extends NodeTest {
  override def apply(node: Node): Boolean = {
    // TODO support namespaces
    name.localPart == "*" || node.label == name.localPart
  }
}

case class NodeTypeTest(nodeType: NodeType) extends NodeTest

sealed trait NodeType

case object CommentNodeType extends NodeType

case object TextNodeType extends NodeType

case object ProcessingInstructionNodeType extends NodeType

case object NodeNodeType extends NodeType

