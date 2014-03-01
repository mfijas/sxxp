package org.sxxp.xpath1.exp

import scala.xml.Node
import org.sxxp.xpath1.parser.expression.Expression

object Evaluator {

  def evaluate(node: Node, expression: Expression) = {

    expression.evaluate(node, XPathContext(node))
  }


}
