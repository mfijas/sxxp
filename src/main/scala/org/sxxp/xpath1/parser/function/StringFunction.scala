package org.sxxp.xpath1.parser.function

import org.sxxp.xpath1.parser.types.XObject
import org.sxxp.xpath1.parser.QName

object StringFunction extends UnaryFunction {
  val QNAME = QName("", "", "string")

  override def apply(arg: XObject) = arg.asString
}
