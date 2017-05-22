class Test {
  checkInt() : Int { 1 };
  checkTrue() : Bool { true };
  checkFalse() : Bool { false };
  checkString() : String { "Test" };

  checkAssignInt(a : Int) : Int { a <- 123 };
  checkAssignBool(b: Bool) : Bool { b <- true };
  checkAssignString(c : String) : String { c <- "Test" };
};
