package org.sxxp.xpath1.parser.path

import org.sxxp.xpath1.parser.step.Step
import scala.xml.{NodeSeq, Node}
import org.sxxp.xpath1.exp.XPathContext

trait LocationPath {
  val steps: List[Step]

  def :+(step: Step): LocationPath

  def select(currentNode: Node, context: XPathContext): NodeSeq
}
