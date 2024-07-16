package refined

import refined.generic.Equal

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import _root_.pureconfig.ConfigConvert

class pureconfigTests extends AnyFunSpec with Matchers {
  it("should create a ConfigConvert for a simple Refined type") {
    val convert = pureconfig.refineConfigConvert[Int, Equal[1]]
  }

  it("should produce the same config value as underlying type") {
    val convert = pureconfig.refineConfigConvert[Int, Equal[1]]
    assert(convert.to(1) == summon[ConfigConvert[Int]].to(1))
  }
}