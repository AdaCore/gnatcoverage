package body Andcor is

   type Expr is record
      Value : Boolean;
   end record;

   function Orelse (B, C : Boolean) return Boolean is
   begin
      return B or else C; -- # orelse :o/e:
   end;

   procedure Eval_F (A, B, C : Boolean; E : out Boolean) is
      Ex : Expr := (Value => A and then Orelse (B, C));  -- # andthen :o/e:
   begin
      E := Ex.Value;  -- # returnValue
   end;

   function F (A, B, C : Boolean) return Boolean is
      Value : Boolean;
   begin
      Eval_F (A, B, C, Value); -- # returnValue
      return Value;            -- # returnValue
   end;

end;



