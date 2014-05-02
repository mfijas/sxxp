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
import org.sxxp.xpath1.parser.util.NodeWithAncestorsUtil.flatNodeSeqToXNodeSeq

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
    val left = flatNodeSeqToXNodeSeq(aNodes, leftXml)

    val rightXml = <x2>
      <a>qwe</a>
      <a>ewq</a>
      <a>abc</a>
    </x2>
    val rightANodes = rightXml \ "a"
    val right = flatNodeSeqToXNodeSeq(rightANodes, rightXml)
    val singleNode = XNodeSeq(Seq(NodeWithAncestors.rootNode(<a>abc</a>)))

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
    val nodeSeq = flatNodeSeqToXNodeSeq(aNodes, xml)
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
    val nodeSeq = flatNodeSeqToXNodeSeq(aNodes, xml)
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
    val nodeSeq = flatNodeSeqToXNodeSeq(aNodes, xml)
    val emptyNodeSeq = XNodeSeq(Seq.empty)
    val xTrue = XBoolean(true)
    val xFalse = XBoolean(false)

    // then
    assert(isEqual(nodeSeq, xTrue) === true)
    assert(isEqual(nodeSeq, xFalse) === false)
    assert(isEqual(emptyNodeSeq, xFalse) === true)
    assert(isEqual(emptyNodeSeq, xTrue) === false)

    assert(isEqual(xTrue, nodeSeq) === true)
    assert(isEqual(xFalse, nodeSeq) === false)
    assert(isEqual(xFalse, emptyNodeSeq) === true)
    assert(isEqual(xTrue, emptyNodeSeq) === false)
  }

  it should "compare anything with XBoolean" in {
    // given
    val xTrue = XBoolean(true)
    val xFalse = XBoolean(false)
    val string = XString("aaaa")
    val number = XNumber(333)

    // then
    assert(isEqual(xTrue, xTrue) === true)
    assert(isEqual(xTrue, string) === true)
    assert(isEqual(xTrue, number) === true)

    assert(isEqual(string, xTrue) === true)
    assert(isEqual(number, xTrue) === true)

    assert(isEqual(xTrue, xFalse) === false)
    assert(isEqual(xFalse, string) === false)
    assert(isEqual(xFalse, number) === false)

    assert(isEqual(string, xFalse) === false)
    assert(isEqual(number, xFalse) === false)
  }

}
