package org.sxxp.xpath1.parser.util

import org.sxxp.xpath1.parser.types.XNodeSeq
import org.sxxp.xpath1.parser.axis.{NodePath, NodeWithAncestors}
import scala.xml.{Node, NodeSeq}

object NodeWithAncestorsUtil {

  /**
   * Converts NodeSeq to XNodeSeq assuming that all nodes in NodeSeq are direct children of one parent node.
   * @param nodes NodeSeq to be converted
   * @param parent Node which is a direct parent of all the nodes in NodeSeq
   */
  def flatNodeSeqToXNodeSeq(nodes: NodeSeq, parent: Node) =
    XNodeSeq(nodes.zipWithIndex.map {
      case (node, index) => NodeWithAncestors(node, List(parent), NodePath(index))
    })
}
