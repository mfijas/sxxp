package org.sxxp.xpath1.parser.function

import org.sxxp.xpath1.parser.types.XObject
import org.sxxp.xpath1.parser.QName

object BooleanFunction extends UnaryFunction {
  val QNAME = QName("", "", "boolean")

  override def apply(arg: XObject): XObject = arg.asBoolean
}
