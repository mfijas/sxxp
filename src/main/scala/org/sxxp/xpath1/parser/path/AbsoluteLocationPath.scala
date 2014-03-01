package org.sxxp.xpath1.parser.path

import org.sxxp.xpath1.parser.step.Step
import scala.xml.{TopScope, NodeSeq, Node, Elem}
import org.sxxp.xpath1.exp.XPathContext
import org.sxxp.xpath1.utils.Logging


/**
 * Location path starting with "/"
 */
case class AbsoluteLocationPath(steps: List[Step]) extends LocationPath with Logging {
  override def :+(step: Step): LocationPath = AbsoluteLocationPath(steps :+ step)

  // TODO not so nice, think about doing it better
  private def dummyRoot(child: Node) =
    Elem(null, "dummy-root", scala.xml.Null, TopScope, false, child)

  override def select(currentNode: Node, context: XPathContext): NodeSeq = {
    var curNodeSeq: NodeSeq = dummyRoot(context.rootNode)

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

object AbsoluteLocationPath {
  def apply(path: LocationPath): AbsoluteLocationPath = AbsoluteLocationPath(path.steps)
}
