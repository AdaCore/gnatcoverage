package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
      E : Boolean := A or else B;  -- # orelse
   begin
      return E;  -- # retVal
   end;
end;



