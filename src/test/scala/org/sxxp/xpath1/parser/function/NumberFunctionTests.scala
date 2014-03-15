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

package org.sxxp.xpath1.parser.function

import org.scalatest.FlatSpec
import org.sxxp.xpath1.parser.types.{XNodeSeq, XBoolean, XString, XNumber}
import org.sxxp.xpath1.parser.axis.NodeWithAncestors

class NumberFunctionTests extends FlatSpec {

  "NumberFunction" should "return XNumber(1) for XString('1.0')" in {
    // given
    val arg = XString("1.0")

    // when
    val result = NumberFunction(arg)

    // then
    assert(result === XNumber(1))
  }

  it should "return XNumber(-3.2) for XString('  -3.2')" in {
    // given
    val arg = XString("  -3.2")

    // when
    val result = NumberFunction(arg)

    // then
    assert(result === XNumber(-3.2))
  }

  it should "return XNumber(1) for XBoolean(true)" in {
    // given
    val arg = XBoolean(true)

    // when
    val result = NumberFunction(arg)

    // then
    assert(result === XNumber(1))
  }

  it should "return XNumber(0) for XBoolean(false)" in {
    // given
    val arg = XBoolean(false)

    // when
    val result = NumberFunction(arg)

    // then
    assert(result === XNumber(0))
  }

  it should "return XNumber(123) for XNumber(123)" in {
    // given
    val arg = XNumber(123)

    // when
    val result = NumberFunction(arg)

    // then
    assert(result === arg)
  }

  it should "return XNumber for parsed string value of first node in the nodeSeq" in {
    // given
    val xml = <x1>
      <a>123</a>
      <a>456</a>
      <a>789</a>
    </x1>
    val aNodes = xml \ "a"
    val nodeSeq = XNodeSeq(aNodes.map(NodeWithAncestors(_, List(xml))))

    // when
    val result = NumberFunction(nodeSeq)

    // then
    assert(result === XNumber(123))
  }

}
