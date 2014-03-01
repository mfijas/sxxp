package org.sxxp.xpath1.parser.path

import scala.xml.{NodeSeq, Node}
import org.sxxp.xpath1.parser.step.Step
import org.sxxp.xpath1.exp.XPathContext
import org.sxxp.xpath1.utils.Logging


case class RelativeLocationPath(steps: List[Step]) extends LocationPath with Logging {
  def :+(step: Step) = RelativeLocationPath(steps :+ step)

  override def select(currentNode: Node, context: XPathContext): NodeSeq = {
    var curNodeSeq: NodeSeq = currentNode
    // TODO verify this if clause
    for (step <- steps if !curNodeSeq.isEmpty) {
      logger.debug("select: step = {}", step)
      curNodeSeq = curNodeSeq.flatMap {
        node =>
          logger.debug(s"select: node = {}", node)
          step.select(node, context)
      }
    }
    curNodeSeq
  }

}
