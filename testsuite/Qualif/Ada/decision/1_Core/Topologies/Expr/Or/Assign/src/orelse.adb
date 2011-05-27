package body Orelse is
   function Or_Else (A, B : Boolean) return Boolean is
      E : Boolean;
   begin
      E := A or else B;  -- # orelse
      return E;          -- # retVal
   end;
end;

