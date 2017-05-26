class Test {
  checkInt() : Int { 1 };
  checkTrue() : Bool { true };
  checkFalse() : Bool { false };
  checkString() : String { "Test" };

  checkId(a : Int) : Int { a };

  checkNew() : String { new String };

  checkSeq() : Bool { {123; false; true; "Test"; true;} };

  checkLet() : String { Let a : String <- "foo" in a };

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
