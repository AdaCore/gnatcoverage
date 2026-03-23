-- Disable the pragmas we will use downstream

pragma Check_Policy (Assertion, Off);

package body Pragmas is

   procedure Log_Debug is
   begin
      Debug_Events := Debug_Events + 1; -- # log
   end;

   procedure Sum (X, Y, UB : Integer; R : out Integer) is
   begin
      pragma Debug (Log_Debug); -- # debug
      pragma Assert (X + Y <= UB); -- # assert
      R := X + Y; -- # eval
   end;

   procedure Dif (X, Y, LB : Integer; R : out Integer) is
   begin
      pragma Debug (Log_Debug); -- # debug
      R := X - Y; -- # eval
      pragma Assert (X - Y >= LB);  -- # assert
   end;
end;
