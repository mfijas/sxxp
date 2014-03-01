/*
 * Copyright 2014 Michał Fijas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
