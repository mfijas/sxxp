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

import scala.xml.NodeSeq


trait XObject {
  def asBoolean: XBoolean

  def asNodeSeq: XNodeSeq = ???

  def asString: XString

  def asNumber: XNumber
}


case class XNumber(value: Double) extends XObject {
  override def asBoolean = XBoolean(!value.isNaN && value != 0)

  // TODO make it conform to spec
  // possibly use ugly code from com.sun.org.apache.xpath.internal.objects.XNumber.str()
  override def asString =
    if (value.toLong.toDouble == value)
      XString(value.toLong.toString)
    else
      XString(value.toString)

  override def asNumber = this
}

case class XNodeSeq(value: NodeSeq) extends XObject {
  override def asNodeSeq = this

  override def asBoolean = XBoolean(!value.isEmpty)

  override def asString = XString(if (value.isEmpty) "" else value(0).text)

  override def asNumber = asString.asNumber

  def isEqualTo(other: XNodeSeq) = {
    def simpleComparison =
      value.text == other.value.text

    def complexComparison = {
      /*
          From spec: comparison will be true if and only if there is a node in the first node-set
          and a node in the second node-set such that the result of performing the comparison
          on the string-values of the two nodes is true.
       */
      // TODO check if this optimization is right
      val (shorter, longer) =
        if (value.length < other.value.length) (value, other.value) else (other.value, value)
      val shorterTexts = shorter.map(_.text).toSet
      longer.exists(node => shorterTexts.contains(node.text))
    }

    if (value.length == 1 && other.value.length == 1) simpleComparison else complexComparison
  }

  def isEqualTo(other: XNumber) =
    value.exists(node => XString(node.text).asNumber == other)

  def isEqualTo(other: XString) =
    value.exists(node => node.text == other.value)

  def isEqualTo(other: XBoolean) =
    asBoolean == other
}

case class XBoolean(value: Boolean) extends XObject {
  override def asBoolean = this

  override def asNumber = XNumber(if (value) 1 else 0)

  override def asString = XString(value.toString)
}

case class XString(value: String) extends XObject {
  override def asString = this

  override def asBoolean = XBoolean(!value.isEmpty)

  override def asNumber = {
    // TODO verify with specification
    XNumber(
      try {
        value.toDouble
      } catch {
        case _: NumberFormatException => Double.NaN
      }
    )
  }
}
