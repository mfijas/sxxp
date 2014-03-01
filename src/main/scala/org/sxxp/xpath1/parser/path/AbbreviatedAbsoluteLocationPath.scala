package org.sxxp.xpath1.parser.path

import org.sxxp.xpath1.parser.step.Step
import scala.xml.{Node, NodeSeq}
import org.sxxp.xpath1.exp.XPathContext
import org.sxxp.xpath1.utils.Logging

/**
 * Location path starting with "//"
 */
case class AbbreviatedAbsoluteLocationPath(steps: List[Step]) extends LocationPath with Logging {
  override def :+(step: Step): LocationPath = AbbreviatedAbsoluteLocationPath(steps :+ step)

  override def select(currentNode: Node, context: XPathContext): NodeSeq = {
    var curNodeSeq: NodeSeq = context.rootNode.descendant_or_self
    // TODO verify this if clause
    for (step <- steps if !curNodeSeq.isEmpty) {
      logger.debug("select: step = {}", step)
      curNodeSeq = curNodeSeq.flatMap {
        node =>
          logger.debug("select: node = {}", node)
          step.select(node, context)
      }
    }
    curNodeSeq
  }
}

object AbbreviatedAbsoluteLocationPath {
  def apply(path: LocationPath): AbbreviatedAbsoluteLocationPath = AbbreviatedAbsoluteLocationPath(path.steps)
}
