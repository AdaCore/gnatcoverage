package body PandPor is

   type My_Bool is record
      Value : Boolean;
   end record;

   function F (A, B, C : My_Bool) return Boolean is
   begin
      return (A.Value and then B.Value) or else C.Value; -- # evalStmt
   end;

   function F (A, B, C : Boolean) return Boolean is
   begin
      return F ((Value => A), (Value => B), (Value => C));  -- # returnValue
   end;
end;
