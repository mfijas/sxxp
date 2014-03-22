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

import org.sxxp.xpath1.parser.axis.NodeWithAncestors
import scala.xml.NodeSeq

trait XObject {
  def asXBoolean: XBoolean

  def asXNodeSeq: XNodeSeq = ???

  def asXString: XString

  def asXNumber: XNumber
}


case class XNumber(value: Double) extends XObject {
  override def asXBoolean = XBoolean(!value.isNaN && value != 0)

  // TODO make it conform to spec
  // possibly use ugly code from com.sun.org.apache.xpath.internal.objects.XNumber.str()
  override def asXString =
    if (value.toLong.toDouble == value)
      XString(value.toLong.toString)
    else
      XString(value.toString)

  override def asXNumber = this
}

case class XNodeSeq(value: Seq[NodeWithAncestors]) extends XObject {
  def toNodeSeq = NodeSeq.fromSeq(value.map(_.node))

  override def asXNodeSeq = this

  override def asXBoolean = XBoolean(!value.isEmpty)

  // TODO check if it's correct or add reference to spec
  override def asXString = XString(if (value.isEmpty) "" else value.head.node.text)

  override def asXNumber = asXString.asXNumber

  def text = NodeWithAncestorsSeq.extractText(value)

  def isEqualTo(other: XNodeSeq) = {
    def simpleComparison =
      text == other.text

    def complexComparison = {
      /*
          From spec: comparison will be true if and only if there is a node in the first node-set
          and a node in the second node-set such that the result of performing the comparison
          on the string-values of the two nodes is true.
       */
      // TODO check if this optimization is right
      val (shorter, longer) =
        if (value.length < other.value.length) (value, other.value) else (other.value, value)
      val shorterTexts = shorter.map(_.node.text).toSet
      longer.exists(n => shorterTexts.contains(n.node.text))
    }

    if (value.length == 1 && other.value.length == 1) simpleComparison else complexComparison
  }

  def isEqualTo(other: XNumber) =
    value.exists(n => XString(n.node.text).asXNumber == other)

  def isEqualTo(other: XString) =
    value.exists(n => n.node.text == other.value)

  def isEqualTo(other: XBoolean) =
    asXBoolean == other
}

case class XBoolean(value: Boolean) extends XObject {
  override def asXBoolean = this

  override def asXNumber = XNumber(if (value) 1 else 0)

  override def asXString = XString(value.toString)
}

case class XString(value: String) extends XObject {
  override def asXString = this

  override def asXBoolean = XBoolean(value.nonEmpty)

  override def asXNumber = {
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
