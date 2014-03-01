package org.sxxp.xpath1.parser.function

import org.sxxp.xpath1.parser.types.XBoolean
import org.sxxp.xpath1.parser.QName

object TrueFunction extends ParameterlessFunction {
  val QNAME = QName("", "", "true")

  override def apply(v1: Unit) = XBoolean(true)
}
