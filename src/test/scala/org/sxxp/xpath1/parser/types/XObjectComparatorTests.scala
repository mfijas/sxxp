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

import org.scalatest.FlatSpec
import org.sxxp.xpath1.parser.axis.NodeWithAncestors

class XObjectComparatorTests extends FlatSpec {

  import XObjectComparator.isEqual

  "XObjectComparator" should "compare two nodeSeqs" in {
    // given
    val leftXml = <x1>
      <a>poi</a>
      <a>abc</a>
      <a>iop</a>
    </x1>
    val aNodes = leftXml \ "a"
    val left = XNodeSeq(aNodes.map(NodeWithAncestors(_, List(leftXml))))

    val rightXml = <x2>
      <a>qwe</a>
      <a>ewq</a>
      <a>abc</a>
    </x2>
    val rightANodes = rightXml \ "a"
    val right = XNodeSeq(
      rightANodes.map(NodeWithAncestors(_, List(rightXml))))
    val singleNode = XNodeSeq(Seq(NodeWithAncestors(<a>abc</a>, List.empty)))

    // then
    assert(isEqual(left, right))
    assert(isEqual(right, left))
    assert(isEqual(left, singleNode))
    assert(isEqual(singleNode, singleNode))
  }

  it should "compare nodeSeq and number" in {
    // given
    val xml = <x1>
      <a>123</a>
      <a>234</a>
      <a>345</a>
    </x1>
    val aNodes = xml \ "a"
    val nodeSeq = XNodeSeq(aNodes.map(NodeWithAncestors(_, List(xml))))
    val number = XNumber(234)
    val otherNumber = XNumber(111)

    // then
    assert(isEqual(nodeSeq, number) === true)
    assert(isEqual(nodeSeq, otherNumber) === false)
    assert(isEqual(number, nodeSeq) === true)
    assert(isEqual(otherNumber, nodeSeq) === false)
  }

  it should "compare nodeSeq and string" in {
    val xml = <x1>
      <a>abc</a>
      <a>bcd</a>
      <a>efg</a>
    </x1>
    val aNodes = xml \ "a"
    // given
    val nodeSeq = XNodeSeq(aNodes.map(NodeWithAncestors(_, List(xml))))
    val string = XString("bcd")
    val otherString = XString("aaa")

    // then
    assert(isEqual(nodeSeq, string) === true)
    assert(isEqual(nodeSeq, otherString) === false)
    assert(isEqual(string, nodeSeq) === true)
    assert(isEqual(otherString, nodeSeq) === false)
  }

  it should "compare nodeSeq and boolean" in {
    // given
    val xml = <x1>
      <a>abc</a>
      <a>bcd</a>
      <a>efg</a>
    </x1>
    val aNodes = xml \ "a"
    val nodeSeq = XNodeSeq(aNodes.map(NodeWithAncestors(_, List(xml))))
    val emptyNodeSeq = XNodeSeq(Seq.empty)
    val xtrue = XBoolean(true)
    val xfalse = XBoolean(false)

    // then
    assert(isEqual(nodeSeq, xtrue) === true)
    assert(isEqual(nodeSeq, xfalse) === false)
    assert(isEqual(emptyNodeSeq, xfalse) === true)
    assert(isEqual(emptyNodeSeq, xtrue) === false)

    assert(isEqual(xtrue, nodeSeq) === true)
    assert(isEqual(xfalse, nodeSeq) === false)
    assert(isEqual(xfalse, emptyNodeSeq) === true)
    assert(isEqual(xtrue, emptyNodeSeq) === false)
  }

  it should "compare anything with XBoolean" in {
    // given
    val xtrue = XBoolean(true)
    val xfalse = XBoolean(false)
    val string = XString("aaaa")
    val number = XNumber(333)

    // then
    assert(isEqual(xtrue, xtrue) === true)
    assert(isEqual(xtrue, string) === true)
    assert(isEqual(xtrue, number) === true)

    assert(isEqual(string, xtrue) === true)
    assert(isEqual(number, xtrue) === true)

    assert(isEqual(xtrue, xfalse) === false)
    assert(isEqual(xfalse, string) === false)
    assert(isEqual(xfalse, number) === false)

    assert(isEqual(string, xfalse) === false)
    assert(isEqual(number, xfalse) === false)
  }

}
