package body Orelse is
   function Or_Else (A, B : Boolean) return Boolean is
      E : Boolean; -- # decl
   begin
      E := A or else B;  -- # orelse :o/0:
      return E;          -- # retVal
   end;
end;

