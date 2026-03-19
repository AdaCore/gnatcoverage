pragma Ada_2012;

package body Twisters is
   function Identity (B : Boolean) return Boolean is
   begin
      --  The goal is to have conditional branches coming from other
      --  functions inlined within decisions. The conditional expression
      --  is there to force the production of conditional branches.
      return (if B then True else False);
   end;
end;
