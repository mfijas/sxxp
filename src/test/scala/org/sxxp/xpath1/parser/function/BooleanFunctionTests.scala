package org.sxxp.xpath1.parser.function

import org.scalatest.FlatSpec
import org.sxxp.xpath1.parser.types.{XString, XNodeSeq, XNumber, XBoolean}
import scala.xml.NodeSeq

class BooleanFunctionTests extends FlatSpec {

  val TRUE = XBoolean(true)
  val FALSE = XBoolean(false)

  "BooleanFunction" should "return XBoolean(true) for XBoolean(true)" in {
    // given
    val arg = TRUE

    // when
    val result = BooleanFunction(arg)

    // then
    assert(result === arg)
  }

  it should "return XBoolean(false) for XBoolean(false)" in {
    // given
    val arg = FALSE

    // when
    val result = BooleanFunction(arg)

    // then
    assert(result === arg)
  }

  it should "return XBoolean(false) for XNumber(NaN)" in {
    // given
    val arg = XNumber(Double.NaN)

    // when
    val result = BooleanFunction(arg)

    // then
    assert(result === FALSE)
  }

  it should "return XBoolean(false) for +0.0" in {
    // given
    val arg = XNumber(+0.0)

    // when
    val result = BooleanFunction(arg)

    // then
    assert(result === FALSE)
  }

  it should "return XBoolean(false) for -0.0" in {
    // given
    val arg = XNumber(-0.0)

    // when
    val result = BooleanFunction(arg)

    // then
    assert(result === FALSE)
  }

  it should "return XBoolean(true) for some non-zero number" in {
    // given
    val arg = XNumber(3)

    // when
    val result = BooleanFunction(arg)

    // then
    assert(result === TRUE)
  }

  val xml =
    <root>
      <a></a> <a/>
    </root>

  it should "return XBoolean(true) for non-empty nodeseq" in {
    // given
    val arg = XNodeSeq(xml \ "a")

    // when
    val result = BooleanFunction(arg)

    // then
    assert(result === TRUE)
  }

  it should "return XBoolean(false) for empty nodeseq" in {
    // given
    val arg = XNodeSeq(NodeSeq.Empty)

    // when
    val result = BooleanFunction(arg)

    // then
    assert(result === FALSE)
  }

  it should "return XBoolean(true) for non-empty string" in {
    // given
    val arg = XString("non-empty string")

    // when
    val result = BooleanFunction(arg)

    // then
    assert(result === TRUE)
  }

  it should "return XBoolean(false) for empty string" in {
    // given
    val arg = XString("")

    // when
    val result = BooleanFunction(arg)

    // then
    assert(result === FALSE)
  }
}
