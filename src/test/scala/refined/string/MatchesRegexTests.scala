package refined.string

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MatchesRegexTests extends AnyFunSpec with Matchers {
  it("should work with string literals") {
    val str: String Refined MatchesRegex["^[A-Za-z]*"] = "Hello"
    assert(str.value == "Hello")
  }

  it("should fail compilation for invalid cases") {
    assertDoesNotCompile("""val str: String Refined MatchesRegex["^[A-Za-z]*"] = "Hello, world!"""")
    assertDoesNotCompile("""val str: String Refined MatchesRegex["^[A-Za-z]*"] = "1"""")
  }

  it("should validate at runtime correctly") {
    assert(Refined.refineV[String, MatchesRegex["^[A-Za-z]*"]]("Hello").isRight)
    assert(Refined.refineV[String, MatchesRegex["^[A-Za-z]*"]]("123").isLeft)
  }
}