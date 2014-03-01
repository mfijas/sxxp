package org.sxxp.xpath1.parser.function

import org.scalatest.FlatSpec
import org.sxxp.xpath1.parser.types.{XNodeSeq, XBoolean, XString, XNumber}

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
    val nodeSeq = XNodeSeq(
      <x1>
        <a>123</a>
        <a>456</a>
        <a>789</a>
      </x1> \ "a"
    )

    // when
    val result = NumberFunction(nodeSeq)

    // then
    assert(result === XNumber(123))
  }

}
