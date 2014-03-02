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

package org.sxxp.xpath1.parser

import org.scalatest.FlatSpec
import org.sxxp.xpath1.parser.expression._
import org.sxxp.xpath1.parser.step.{CurNodeStep, AbbreviatedNodeStep, NodeStep}
import org.sxxp.xpath1.parser.nodetest.{NameNodeTest, NodeTest}
import org.sxxp.xpath1.parser.path.{AbsoluteLocationPath, AbbreviatedAbsoluteLocationPath, RelativeLocationPath}


class XPathParserTests extends FlatSpec {

  val parser = new XPathParser

  def verifyParseResult[T](production: parser.Parser[T], input: String, expected: Any) {
    val parseResult = parser.parseAll(production, input)
    parseResult match {
      case parser.Success(result, _) =>
        assert(result === expected)
      case parser.NoSuccess(msg, next) =>
        fail(msg)
    }
  }

  def verifyNameTestParseResult(input: String, expected: QName) = verifyParseResult(parser.nameTest, input, expected)

  def verifyNodeTestParseResult(input: String, expected: NodeTest) = verifyParseResult(parser.nodeTest, input, expected)

  def verifyPathExprParseResult(input: String, expected: Expression) = verifyParseResult(parser.pathExpr, input, expected)


  "nameTest" should "parse non-qualified name" in {
    verifyNameTestParseResult("someNode", QName("someNode"))
  }

  it should "parse qualified name" in {
    verifyNameTestParseResult("myns:someNode", QName("", "myns", "someNode"))
  }

  it should "parse any name (*)" in {
    verifyNameTestParseResult("*", QName("*"))
  }

  it should "parse any name for a given namespace (ns:*)" in {
    verifyNameTestParseResult("myns:*", QName("", "myns", "*"))
  }

  "nodeTest" should "parse qualified name" in {
    verifyNodeTestParseResult("myns:someNode", NameNodeTest(QName("", "myns", "someNode")))
  }

  "pathExpr" should "parse a" in {
    verifyPathExprParseResult("a",
      LocationPathExpression(
        RelativeLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List())))))
  }

  it should "parse a/b" in {
    verifyPathExprParseResult("a/b",
      LocationPathExpression(
        RelativeLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List()),
            NodeStep(NameNodeTest(QName("b")), List())))))
  }

  it should "parse a//b" in {
    verifyPathExprParseResult("a//b",
      LocationPathExpression(
        RelativeLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List()),
            AbbreviatedNodeStep(
              NodeStep(NameNodeTest(QName("b")), List()))))))
  }

  it should "parse /a/b" in {
    verifyPathExprParseResult("/a/b",
      LocationPathExpression(
        AbsoluteLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List()),
            NodeStep(NameNodeTest(QName("b")), List())))))
  }

  it should "parse //a/b" in {
    verifyPathExprParseResult("//a/b",
      LocationPathExpression(
        AbbreviatedAbsoluteLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List()),
            NodeStep(NameNodeTest(QName("b")), List())))))
  }

  it should "parse a[1]" in {
    verifyPathExprParseResult("a[1]",
      LocationPathExpression(
        RelativeLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List(Predicate(NumberExpression(1.0))))))))
  }

  it should "parse ." in {
    verifyPathExprParseResult(".",
      LocationPathExpression(
        RelativeLocationPath(
          List(CurNodeStep))))
  }

  it should "parse a[1]/b[. = 'abcd']" in {
    verifyPathExprParseResult("a[1]/b[.='abcd']",
      LocationPathExpression(
        RelativeLocationPath(
          List(
            NodeStep(NameNodeTest(QName("a")), List(Predicate(NumberExpression(1.0)))),
            NodeStep(NameNodeTest(QName("b")), List(Predicate(EqExpression(LocationPathExpression(RelativeLocationPath(List(CurNodeStep))), LiteralExpression("abcd"))))))))
    )
  }

  it should "parse a//." in {
    verifyPathExprParseResult("a//.",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(NameNodeTest(QName("a")), List()), AbbreviatedNodeStep(CurNodeStep)))))
  }

  it should "parse (//aa)[1]" in {
    verifyPathExprParseResult("(//aa)[1]",
      FilterExpression(
        LocationPathExpression(
          AbbreviatedAbsoluteLocationPath(List(NodeStep(NameNodeTest(QName("aa")), List())))
        ), Predicate(NumberExpression(1.0))))
  }

  it should "parse /*" in {
    verifyPathExprParseResult("/*",
      LocationPathExpression(AbsoluteLocationPath(List(NodeStep(NameNodeTest(QName("*")), List())))))
  }

  it should "parse /a[. = myFun(1,//somePath)]" in {
    verifyPathExprParseResult("/a[. = myFun(1,//somePath)]",
      LocationPathExpression(
        AbsoluteLocationPath(
          List(NodeStep(NameNodeTest(QName("a")), List(
            Predicate(
              EqExpression(
                LocationPathExpression(RelativeLocationPath(List(CurNodeStep))),
                FunctionCallExpression(QName("myFun"), List(
                  NumberExpression(1.0),
                  LocationPathExpression(
                    AbbreviatedAbsoluteLocationPath(List(NodeStep(NameNodeTest(QName("somePath")), List()))))))))))))))
  }

  it should "parse (a)/b" in {
    verifyPathExprParseResult("(a)/b",
      PathExpression(
        LocationPathExpression(RelativeLocationPath(List(NodeStep(NameNodeTest(QName("a")), List())))),
        RelativeLocationPath(List(NodeStep(NameNodeTest(QName("b")), List())))))
  }

  it should "parse (a)//b" in {
    verifyPathExprParseResult("(a)//b",
      AbbreviatedPathExpression(
        LocationPathExpression(RelativeLocationPath(List(NodeStep(NameNodeTest(QName("a")), List())))),
        RelativeLocationPath(List(NodeStep(NameNodeTest(QName("b")), List())))))
  }

}
