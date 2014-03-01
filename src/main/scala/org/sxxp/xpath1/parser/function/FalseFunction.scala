package org.sxxp.xpath1.parser.function

import org.sxxp.xpath1.parser.types.XBoolean
import org.sxxp.xpath1.parser.QName

object FalseFunction extends ParameterlessFunction {
  val QNAME = QName("", "", "false")

  override def apply(unit: Unit) = XBoolean(false)
}
