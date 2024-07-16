package refined.types

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class stringTests extends AnyFunSpec with Matchers with string {
  describe("NonEmptyString") {
    it("should typecheck for a valid case") {
      val str: NonEmptyString = "Hello"
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val str: NonEmptyString = """"")
    }
  }

  describe("FiniteString") {
    it("should typecheck for a valid case") {
      val str: FiniteString[10] = "Hello"
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val str: FiniteString[3] = "Hello"""")
    }
  }

  describe("NonEmptyFiniteString") {
    it("should typecheck for a valid case") {
      val str: NonEmptyFiniteString[10] = "Hello"
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("""val str: NonEmptyFiniteString[3] = "Hello"""")
      assertDoesNotCompile("""val str: NonEmptyFiniteString[3] = """"")
    }
  }
}