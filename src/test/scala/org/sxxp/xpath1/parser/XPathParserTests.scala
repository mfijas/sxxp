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
import org.sxxp.xpath1.parser.step.{ParentNodeStep, CurNodeStep}
import org.sxxp.xpath1.parser.nodetest.NodeTest
import org.sxxp.xpath1.parser.path.{AbsoluteLocationPath, AbbreviatedAbsoluteLocationPath}
import org.sxxp.xpath1.parser.axis._
import org.sxxp.xpath1.parser.expression.PathExpression
import org.sxxp.xpath1.parser.expression.AbbreviatedPathExpression
import org.sxxp.xpath1.parser.expression.LiteralExpression
import org.sxxp.xpath1.parser.nodetest.NameNodeTest
import org.sxxp.xpath1.parser.expression.FunctionCallExpression
import org.sxxp.xpath1.parser.path.RelativeLocationPath
import org.sxxp.xpath1.parser.expression.EqExpression
import org.sxxp.xpath1.parser.step.NodeStep
import org.sxxp.xpath1.parser.expression.LocationPathExpression
import org.sxxp.xpath1.parser.expression.FilterExpression
import org.sxxp.xpath1.parser.expression.NumberExpression
import org.sxxp.xpath1.parser.step.AbbreviatedNodeStep


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
            NodeStep(ChildAxis, NameNodeTest(QName("a")), List())))))
  }

  it should "parse a/b" in {
    verifyPathExprParseResult("a/b",
      LocationPathExpression(
        RelativeLocationPath(
          List(
            NodeStep(ChildAxis, NameNodeTest(QName("a")), List()),
            NodeStep(ChildAxis, NameNodeTest(QName("b")), List())))))
  }

  it should "parse a//b" in {
    verifyPathExprParseResult("a//b",
      LocationPathExpression(
        RelativeLocationPath(
          List(
            NodeStep(ChildAxis, NameNodeTest(QName("a")), List()),
            AbbreviatedNodeStep(
              NodeStep(ChildAxis, NameNodeTest(QName("b")), List()))))))
  }

  it should "parse /a/b" in {
    verifyPathExprParseResult("/a/b",
      LocationPathExpression(
        AbsoluteLocationPath(
          List(
            NodeStep(ChildAxis, NameNodeTest(QName("a")), List()),
            NodeStep(ChildAxis, NameNodeTest(QName("b")), List())))))
  }

  it should "parse //a/b" in {
    verifyPathExprParseResult("//a/b",
      LocationPathExpression(
        AbbreviatedAbsoluteLocationPath(
          List(
            NodeStep(ChildAxis, NameNodeTest(QName("a")), List()),
            NodeStep(ChildAxis, NameNodeTest(QName("b")), List())))))
  }

  it should "parse a[1]" in {
    verifyPathExprParseResult("a[1]",
      LocationPathExpression(
        RelativeLocationPath(
          List(
            NodeStep(ChildAxis, NameNodeTest(QName("a")), List(Predicate(NumberExpression(1.0))))))))
  }

  it should "parse ." in {
    verifyPathExprParseResult(".",
      LocationPathExpression(
        RelativeLocationPath(
          List(CurNodeStep))))
  }

  it should "parse .." in {
    verifyPathExprParseResult("..",
      LocationPathExpression(
        RelativeLocationPath(
          List(ParentNodeStep))))
  }

  it should "parse a[1]/b[. = 'abcd']" in {
    verifyPathExprParseResult("a[1]/b[.='abcd']",
      LocationPathExpression(
        RelativeLocationPath(
          List(
            NodeStep(ChildAxis, NameNodeTest(QName("a")), List(Predicate(NumberExpression(1.0)))),
            NodeStep(ChildAxis, NameNodeTest(QName("b")), List(Predicate(EqExpression(LocationPathExpression(RelativeLocationPath(List(CurNodeStep))), LiteralExpression("abcd"))))))))
    )
  }

  it should "parse a//." in {
    verifyPathExprParseResult("a//.",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("a")), List()), AbbreviatedNodeStep(CurNodeStep)))))
  }

  it should "parse (//aa)[1]" in {
    verifyPathExprParseResult("(//aa)[1]",
      FilterExpression(
        LocationPathExpression(
          AbbreviatedAbsoluteLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("aa")), List())))
        ), Predicate(NumberExpression(1.0))))
  }

  it should "parse /*" in {
    verifyPathExprParseResult("/*",
      LocationPathExpression(AbsoluteLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("*")), List())))))
  }

  it should "parse /a[. = myFun(1,//somePath)]" in {
    verifyPathExprParseResult("/a[. = myFun(1,//somePath)]",
      LocationPathExpression(
        AbsoluteLocationPath(
          List(NodeStep(ChildAxis, NameNodeTest(QName("a")), List(
            Predicate(
              EqExpression(
                LocationPathExpression(RelativeLocationPath(List(CurNodeStep))),
                FunctionCallExpression(QName("myFun"), List(
                  NumberExpression(1.0),
                  LocationPathExpression(
                    AbbreviatedAbsoluteLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("somePath")), List()))))))))))))))
  }

  it should "parse (a)/b" in {
    verifyPathExprParseResult("(a)/b",
      PathExpression(
        LocationPathExpression(RelativeLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("a")), List())))),
        RelativeLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("b")), List())))))
  }

  it should "parse (a)//b" in {
    verifyPathExprParseResult("(a)//b",
      AbbreviatedPathExpression(
        LocationPathExpression(RelativeLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("a")), List())))),
        RelativeLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("b")), List())))))
  }

  it should "parse name containing hyphen 'test-node'" in {
    verifyPathExprParseResult("test-node",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("test-node")), List())))))
  }

  it should "parse 'ancestor' axis" in {
    verifyPathExprParseResult("ancestor::element",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(AncestorAxis, NameNodeTest(QName("element")), List())))))
  }

  it should "parse 'ancestor-or-self' axis" in {
    verifyPathExprParseResult("ancestor-or-self::element",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(AncestorOrSelfAxis, NameNodeTest(QName("element")), List())))))
  }

  it should "parse 'attribute' axis" in {
    verifyPathExprParseResult("attribute::attribute",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(AttributeAxis, NameNodeTest(QName("attribute")), List())))))
  }

  it should "parse 'child' axis" in {
    verifyPathExprParseResult("child::element",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(ChildAxis, NameNodeTest(QName("element")), List())))))
  }

  it should "parse 'descendant' axis" in {
    verifyPathExprParseResult("descendant::element",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(DescendantAxis, NameNodeTest(QName("element")), List())))))
  }

  it should "parse 'descendant-or-self' axis" in {
    verifyPathExprParseResult("descendant-or-self::element",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(DescendantOrSelfAxis, NameNodeTest(QName("element")), List())))))
  }

  it should "parse 'following' axis" in {
    verifyPathExprParseResult("following::element",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(FollowingAxis, NameNodeTest(QName("element")), List())))))
  }

  it should "parse 'following-sibling' axis" in {
    verifyPathExprParseResult("following-sibling::element",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(FollowingSiblingAxis, NameNodeTest(QName("element")), List())))))
  }

  it should "parse 'namespace' axis" in {
    verifyPathExprParseResult("namespace::*",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(NamespaceAxis, NameNodeTest(QName("*")), List())))))
  }

  it should "parse 'parent' axis" in {
    verifyPathExprParseResult("parent::element",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(ParentAxis, NameNodeTest(QName("element")), List())))))
  }

  it should "parse 'preceding' axis" in {
    verifyPathExprParseResult("preceding::element",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(PrecedingAxis, NameNodeTest(QName("element")), List())))))
  }

  it should "parse 'preceding-sibling' axis" in {
    verifyPathExprParseResult("preceding-sibling::element",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(PrecedingSiblingAxis, NameNodeTest(QName("element")), List())))))
  }

  it should "parse 'self' axis" in {
    verifyPathExprParseResult("self::element",
      LocationPathExpression(RelativeLocationPath(List(NodeStep(SelfAxis, NameNodeTest(QName("element")), List())))))
  }

}
