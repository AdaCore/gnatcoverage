pragma Check_Policy (Assertion, Off);
pragma Check_Policy (Precondition, Off);
pragma Check_Policy (Postcondition, Off);

package body Asserts is
   procedure Sum (X, Y, UB : Integer; R : out Integer) is
   begin
      pragma Assert (X + Y <= UB);
      R := X + Y; -- # eval
   end;
   
   procedure Dif (X, Y, LB : Integer; R : out Integer) is
   begin
      R := X - Y; -- # eval
      pragma Assert (X - Y >= LB);
   end;
   
end;
