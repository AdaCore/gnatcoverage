
package body Expr is

   procedure If_Andthen
     (A, B : Boolean; X : Integer) is
   begin
      if A and then B then -- # evalAnd
         Value := X;       -- # latchAnd
      end if;
   end;

   procedure If_Orelse
     (A, B : Boolean; X : Integer) is
   begin
      if A or else B then -- # evalOr
         Value := X;      -- # latchOr
      end if;
   end;

end;


