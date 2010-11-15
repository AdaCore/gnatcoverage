function CC6 (t : Integer; A, B, C : Boolean) return Boolean is
   Low  : constant Boolean := t < 10;
   Eq   : constant Boolean := t = 10;
   High : constant Boolean := t > 10;
begin
   return (Low and then A)      -- # line1
     or else (Eq and then B)    -- # line2
     or else (High and then C); -- # line3
end;
