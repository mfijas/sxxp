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

package org.sxxp.xpath1.parser.expression

import org.scalatest.FlatSpec
import org.sxxp.xpath1.parser.QName
import org.sxxp.xpath1.parser.types.{XNumber, XBoolean}
import org.sxxp.xpath1.parser.path.RelativeLocationPath
import org.sxxp.xpath1.parser.step.NodeStep
import org.sxxp.xpath1.parser.nodetest.NameNodeTest
import org.sxxp.xpath1.exp.XPathContext
import org.sxxp.xpath1.parser.axis.ChildAxis

class ExpressionTest extends FlatSpec {

  val True = XBoolean(value = true)
  val False = XBoolean(value = false)

  val TrueExp = FunctionCallExpression(QName("true"), List.empty)
  val FalseExp = FunctionCallExpression(QName("false"), List.empty)

  "FunctionCallExpression" should "allow invoking true() function" in {
    assert(TrueExp.evaluate(null, null) === True)
  }

  it should "allow invoking false() function" in {
    assert(FalseExp.evaluate(null, null) === False)
  }

  "OrExpression" should "have OR logic" in {
    assert(OrExpression(FalseExp, FalseExp).evaluate(null, null) === False)
    assert(OrExpression(TrueExp, FalseExp).evaluate(null, null) === True)
    assert(OrExpression(FalseExp, TrueExp).evaluate(null, null) === True)
    assert(OrExpression(TrueExp, TrueExp).evaluate(null, null) === True)
  }

  "AndExpression" should "have AND logic" in {
    assert(AndExpression(FalseExp, FalseExp).evaluate(null, null) === False)
    assert(AndExpression(TrueExp, FalseExp).evaluate(null, null) === False)
    assert(AndExpression(FalseExp, TrueExp).evaluate(null, null) === False)
    assert(AndExpression(TrueExp, TrueExp).evaluate(null, null) === True)
  }

  "SumExpression" should "add numbers" in {
    assert(SumExpression(NumberExpression(1), NumberExpression(2)).evaluate(null, null) === XNumber(3))
  }

  it should "convert operands to numbers" in {
    assert(SumExpression(LiteralExpression("1"), LiteralExpression("2")).evaluate(null, null) === XNumber(3))
  }

  "SubtractExpression" should "subtract numbers" in {
    assert(SubtractExpression(NumberExpression(1), NumberExpression(2)).evaluate(null, null) === XNumber(-1))
  }

  it should "convert operands to numbers" in {
    assert(SubtractExpression(LiteralExpression("1"), LiteralExpression("2")).evaluate(null, null) === XNumber(-1))
  }

  "MultiplyExpression" should "multiply numbers" in {
    assert(MultiplyExpression(NumberExpression(2), NumberExpression(3)).evaluate(null, null) === XNumber(6))
  }

  it should "convert operands to numbers" in {
    assert(MultiplyExpression(LiteralExpression("2"), LiteralExpression("3")).evaluate(null, null) === XNumber(6))
  }

  "DivExpression" should "divide numbers" in {
    assert(DivExpression(NumberExpression(6), NumberExpression(3)).evaluate(null, null) === XNumber(2))
  }

  it should "convert operands to numbers" in {
    assert(DivExpression(LiteralExpression("6"), LiteralExpression("3")).evaluate(null, null) === XNumber(2))
  }

  "ModExpression" should "calculate modulo" in {
    assert(ModExpression(NumberExpression(6), NumberExpression(4)).evaluate(null, null) === XNumber(2))
  }

  it should "convert operands to numbers" in {
    assert(ModExpression(LiteralExpression("6"), LiteralExpression("4")).evaluate(null, null) === XNumber(2))
  }

  "MinusExpression" should "negate number" in {
    assert(MinusExpression(NumberExpression(123)).evaluate(null, null) === XNumber(-123))
  }

  it should "convert operands to numbers" in {
    assert(MinusExpression(LiteralExpression("123")).evaluate(null, null) === XNumber(-123))
  }

  val xml =
    <root>
      <a>
        <b>
          <c>1</c>
          <c>2</c>
        </b>
        <b>
          <c>3</c>
        </b>
      </a>
    </root>

  "PathExpression" should "evaluate properly" in {
    // given
    val expression = LocationPathExpression(RelativeLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("a")), List()))))
    val path = RelativeLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("b")), List())))
    val pathExpression = PathExpression(expression, path)

    // when
    val result = pathExpression.evaluate(xml, XPathContext(xml))

    // then
    assert(result.value === xml \ "a" \ "b")
  }

  "AbbreviatedPathExpression" should "evaluate properly" in {
    // given
    val expression = LocationPathExpression(RelativeLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("a")), List()))))
    val path = RelativeLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("c")), List())))
    val pathExpression = AbbreviatedPathExpression(expression, path)

    // when
    val result = pathExpression.evaluate(xml, XPathContext(xml))

    // then
    assert(result.value === xml \ "a" \\ "c")
  }

}
