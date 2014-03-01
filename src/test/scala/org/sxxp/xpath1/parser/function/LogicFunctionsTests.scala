package org.sxxp.xpath1.parser.function

import org.scalatest.FlatSpec
import org.sxxp.xpath1.parser.types.XBoolean

class LogicFunctionsTests extends FlatSpec {

  "TrueFunction" should "return XBoolean(true)" in {
    assert(TrueFunction() === XBoolean(true))
  }

  "FalseFunction" should "return XBoolean(false)" in {
    assert(FalseFunction() === XBoolean(false))
  }

}
