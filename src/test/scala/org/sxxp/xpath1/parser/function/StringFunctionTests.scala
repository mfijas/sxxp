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
import org.sxxp.xpath1.parser.types.{XBoolean, XNumber, XString, XNodeSeq}
import org.sxxp.xpath1.parser.util.NodeWithAncestorsUtil.flatNodeSeqToXNodeSeq

class StringFunctionTests extends FlatSpec {

  "StringFunction" should "return XString containing string-value of first node of nodeSeq" in {
    // given

    val xml = <x1>
      <a>abc</a> <a>def</a> <a>ghi</a>
    </x1>
    val aNodes = xml \ "a"
    val nodeSeq = flatNodeSeqToXNodeSeq(aNodes, xml)

    // when
    val result = StringFunction(nodeSeq)

    // then
    assert(result === XString("abc"))
  }

  it should "return XString('') for empty nodeSeq" in {
    // given
    val nodeSeq = XNodeSeq(Seq.empty)

    // when
    val result = StringFunction(nodeSeq)

    // then
    assert(result === XString(""))
  }

  it should "return XString('NaN') for XNumber(NaN)" in {
    // given
    val number = XNumber(Double.NaN)

    // when
    val result = StringFunction(number)

    // then
    assert(result === XString("NaN"))
  }

  it should "return XString('0') for XNumber(0)" in {
    // given
    val number = XNumber(0)

    // when
    val result = StringFunction(number)

    // then
    assert(result === XString("0"))
  }

  it should "return XString('0') for XNumber(-0)" in {
    // given
    val number = XNumber(-0)

    // when
    val result = StringFunction(number)

    // then
    assert(result === XString("0"))
  }

  it should "return XString('Infinity') for XNumber(PositiveInfinity)" in {
    // given
    val number = XNumber(Double.PositiveInfinity)

    // when
    val result = StringFunction(number)

    // then
    assert(result === XString("Infinity"))
  }

  it should "return XString('-Inifinity') for XNumber(NegativeInfinity)" in {
    // given
    val number = XNumber(Double.NegativeInfinity)

    // when
    val result = StringFunction(number)

    // then
    assert(result === XString("-Infinity"))
  }

  it should "return XString('-123') for XNumber(-123)" in {
    // given
    val number = XNumber(-123)

    // when
    val result = StringFunction(number)

    // then
    assert(result === XString("-123"))
  }

  it should "return XString('-0.123') for XNumber(-0.123)" in {
    // given
    val number = XNumber(-0.123)

    // when
    val result = StringFunction(number)

    // then
    assert(result === XString("-0.123"))
  }

  it should "return XString('true') for XBoolean(true)" in {
    // given
    val boolean = XBoolean(true)

    // when
    val result = StringFunction(boolean)

    // then
    assert(result === XString("true"))
  }

  it should "return XString('false') for XBoolean(false)" in {
    // given
    val boolean = XBoolean(false)

    // when
    val result = StringFunction(boolean)

    // then
    assert(result === XString("false"))
  }

}
