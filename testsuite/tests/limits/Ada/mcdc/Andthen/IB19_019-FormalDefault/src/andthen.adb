package body Andthen is

   AA, BB : Boolean;

   function Expr
     (Value : Boolean := AA and then BB) -- # andthen
     return Boolean;

   function Expr
     (Value : Boolean := AA and then BB) -- # andthen
     return Boolean
   is
   begin
      return Value; -- # retVal
   end;

   function And_Then (A, B : Boolean) return Boolean is
   begin
      AA := A;  -- # retVal
      BB := B;  -- # retVal
      return Expr;  -- # retVal
   end;
end;
