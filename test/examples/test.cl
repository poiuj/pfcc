class Foo { };
class Bar { };

class Test {
  checkInt() : Int { 1 };
  checkTrue() : Bool { true };
  checkFalse() : Bool { false };
  checkString() : String { "Test" };

  checkId(a : Int) : Int { a };

  checkNew() : String { new String };

  checkSeq() : Bool { {123; false; true; "Test"; true;} };

  checkLet() : String { Let a : String <- "foo" in a };

  checkNot(a : Bool) : Bool { not a };
  checkComp(a : Int) : Int { ~a };
  checkIsVoid(a : Test) : Bool { isvoid a };

  checkMul(a : Int, b : Int) : Int { a * b };
  checkDiv(a : Int, b : Int) : Int { a / b };
  checkPlus(a : Int, b : Int) : Int { a + b };
  checkMinus(a : Int, b : Int) : Int { a - b };

  checkLe(a : Int, b : Int) : Bool { a <= b };
  checkLt(a : Int, b : Int) : Bool { a < b };

  checkIntEq(a : Int, b : Int) : Bool { a = b };
  checkStringEq(a : String, b : String) : Bool { a = b };
  checkBool(a : Bool, b : Bool) : Bool { a = b};

  checkObjEq(a : Test, b : Test) : Bool { a = b };
  checkObjEq2(a : Foo, b : Bar) : Bool { a = b };

  checkCall(t : Test, a : String, b : String) : Bool {
    t.checkStringEq(a, b)
  };

  checkAssignInt(a : Int) : Int { a <- 123 };
  checkAssignBool(b: Bool) : Bool { b <- true };
  checkAssignString(c : String) : String { c <- "Test" };
  checkAssignId(a : Int, b : Int) : Int { a <- b };
  checkAssignNew(a : String) : String { a <- new String };
  checkAssignSeq(a : Bool) : Bool { a <-
    { 123; false; true; "Test"; false; }
  };
  checkAssignLet(a : String) : String { a <- let b : String <- "bar" in b };
};
