package org.sxxp.xpath1.parser.expression

import org.scalatest.FlatSpec
import org.sxxp.xpath1.parser.QName
import org.sxxp.xpath1.parser.types.{XNumber, XBoolean}

class ExpressionTest extends FlatSpec {

  val True = XBoolean(true)
  val False = XBoolean(false)

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


}
