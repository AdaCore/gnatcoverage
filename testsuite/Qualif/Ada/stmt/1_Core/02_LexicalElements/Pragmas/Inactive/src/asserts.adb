-- Disable the pragmas we will use downstream

pragma Check_Policy (Assertion, Off);

package body Asserts is
   procedure Sum (X, Y, UB : Integer; R : out Integer) is
   begin
      pragma Assert (X + Y <= UB); -- # assert
      R := X + Y; -- # eval
   end;

   procedure Dif (X, Y, LB : Integer; R : out Integer) is
   begin
      R := X - Y; -- # eval
      pragma Assert (X - Y >= LB);  -- # assert
   end;
end;
