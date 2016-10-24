package body Ops is
   pragma Assertion_Policy (Assert => Ignore);
   
   procedure Latch_X (X : Integer) is
   begin
      Last_X := X; -- # stmt
      Latches := Latches + 1; -- # stmt
      pragma Assert (Latches > 0); -- # assert
   end;   
end;
