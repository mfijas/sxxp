package org.sxxp.xpath1.parser.path

import org.sxxp.xpath1.parser.step.Step
import scala.xml.Node
import org.sxxp.xpath1.exp.XPathContext

/**
 * Root element location path: "/"
 */
case object RootLocationPath extends LocationPath {
  override val steps: List[Step] = List.empty

  override def :+(step: Step): LocationPath = throw new IllegalStateException("cannot add step to root location")

  override def select(currentNode: Node, context: XPathContext) = context.rootNode
}
