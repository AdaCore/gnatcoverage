--  This test is not Ada2012 and Assertion_Policy was only allowed
--  as a configuration pragma before that (e.g. Ada 2005).

pragma Assertion_Policy (Assert => Ignore);

package body Ops is
   
   procedure Latch_X (X : Integer) is
   begin
      Last_X := X; -- # stmt
      Latches := Latches + 1; -- # stmt
      pragma Assert (Latches > 0); -- # assert
   end;   
end;
