package body Ranges is

   procedure Check_Range (X, Min, Max : Integer; In_Range : out Boolean) is
   begin
      if X < Min then        -- # checkOmin
         In_Range := False;  -- # outMin
      elsif X > Max then     -- # checkOmax
         In_Range := False;  -- # outMax
      else
         In_Range := True;   -- # inRange
      end if;
   end;
end;

