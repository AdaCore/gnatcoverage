package body Orelse is

   type Expr is record
      Value : Boolean;
   end record;

   function Or_Else (A, B : Boolean) return Boolean is
      E : Expr := (Value => A or else B);  -- # orelse
   begin
      return E.Value;  -- # retVal
   end;
end;



