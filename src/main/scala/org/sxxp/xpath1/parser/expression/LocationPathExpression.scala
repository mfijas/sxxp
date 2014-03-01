package org.sxxp.xpath1.parser.expression

import scala.xml.Node
import org.sxxp.xpath1.exp.XPathContext
import org.sxxp.xpath1.parser.path.LocationPath
import org.sxxp.xpath1.parser.types.{XNodeSeq, XObject}


case class LocationPathExpression(path: LocationPath) extends Expression {
  override def evaluate(node: Node, context: XPathContext): XObject = XNodeSeq(path.select(node, context))
}
