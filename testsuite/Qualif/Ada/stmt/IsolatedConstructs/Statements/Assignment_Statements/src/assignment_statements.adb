package body Assignment_Statements is
   procedure Swap (I, J : in out Integer) is
      Tmp : Integer;
   begin
      Tmp := I;   -- # swap
      I   := J;   -- # swap
      J   := Tmp; -- # swap
   end Swap;

   function Max_Value (I, J : Integer) return Integer is
      Result : Integer;
   begin
      Result := I;       -- # max

      if J > Result then    -- # max
         Result := J;    -- # ifmax
      end if;

      return Result;     -- # max
   end Max_Value;

end Assignment_Statements;

