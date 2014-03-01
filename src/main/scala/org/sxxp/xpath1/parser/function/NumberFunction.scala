package org.sxxp.xpath1.parser.function

import org.sxxp.xpath1.parser.types.XObject
import org.sxxp.xpath1.parser.QName

object NumberFunction extends UnaryFunction {
  val QNAME = QName("", "", "number")

  override def apply(arg: XObject) = arg.asNumber
}
