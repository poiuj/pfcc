class Test {
  checkInt() : Int { 1 };
  checkTrue() : Bool { true };
  checkFalse() : Bool { false };
  checkString() : String { "Test" };

  checkId(a : Int) : Int { a };

  checkNew() : String { new String };

  checkAssignInt(a : Int) : Int { a <- 123 };
  checkAssignBool(b: Bool) : Bool { b <- true };
  checkAssignString(c : String) : String { c <- "Test" };
  checkAssignId(a : Int, b : Int) : Int { a <- b };
  checkAssignNew(a : String) : String { a <- new String };
};
