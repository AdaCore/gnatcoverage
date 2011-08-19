package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
      E : Boolean := A or else B;  -- # orelse :o/0:
   begin
      return E;  -- # retVal
   end;
end;



