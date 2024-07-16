package refined.types

import refined.Refined

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class numericTests extends AnyFunSpec with Matchers with numeric {
  describe("PosInt") {
    it("should typecheck for a valid case") {
      val num: PosInt = 5
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: PosInt = -1")
    }
  }

  describe("NegInt") {
    it("should typecheck for a valid case") {
      val num: NegInt = -5
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: NegInt = 1")
    }
  }

  describe("PosLong") {
    it("should typecheck for a valid case") {
      val num: PosLong = 5L
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: PosLong = -1L")
    }
  }

  describe("NegLong") {
    it("should typecheck for a valid case") {
      val num: NegLong = -5L
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: NegLong = 1L")
    }
  }

  describe("PosFloat") {
    it("should typecheck for a valid case") {
      val num: PosFloat = 5F
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: PosFloat = -1F")
    }
  }

  describe("NegFloat") {
    it("should typecheck for a valid case") {
      val num: NegFloat = -5F
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: NegFloat = 1F")
    }
  }

  describe("PosDouble") {
    it("should typecheck for a valid case") {
      val num: PosDouble = 5D
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: PosDouble = -1D")
    }
  }

  describe("NegDouble") {
    it("should typecheck for a valid case") {
      val num: NegDouble = -5D
    }

    it("should fail compilation for invalid cases") {
      assertDoesNotCompile("val num: NegDouble = 1D")
    }
  }
}