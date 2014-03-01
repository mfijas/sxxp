package org.sxxp.xpath1.parser.types

object XObjectComparator {
  def isEqual(left: XObject, right: XObject) = {
    (left, right) match {
      case (ls: XNodeSeq, rs: XNodeSeq) =>
        ls.isEqualTo(rs)
      case (seq: XNodeSeq, number: XNumber) =>
        seq.isEqualTo(number)
      case (number: XNumber, seq: XNodeSeq) =>
        seq.isEqualTo(number)
      case (seq: XNodeSeq, string: XString) =>
        seq.isEqualTo(string)
      case (string: XString, seq: XNodeSeq) =>
        seq.isEqualTo(string)
      case (seq: XNodeSeq, boolean: XBoolean) =>
        seq.isEqualTo(boolean)
      case (boolean: XBoolean, seq: XNodeSeq) =>
        seq.isEqualTo(boolean)

      case (boolean: XBoolean, obj: XObject) =>
        boolean == obj.asBoolean
      case (obj: XObject, boolean: XBoolean) =>
        boolean == obj.asBoolean

      case (number: XNumber, obj: XObject) =>
        number == obj.asNumber
      case (obj: XObject, number: XNumber) =>
        number == obj.asNumber

      case (left: XObject, right: XObject) =>
        left.toString == right.toString
    }
  }
}
