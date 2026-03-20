pragma Ada_2012;

with Ops; use Ops;

package body Sys is
   function If0 (A, B, C : Boolean) return Boolean is
   begin
      return (if A then B else C); -- # ifa
   end;

   function If1 (A, B, C : Boolean) return Boolean is
      Op : Op_Self_T;
   begin
      return (if Eval (Op, A) then B else C); -- # ifa
   end;

   function Ifnot0 (A, B, C : Boolean) return Boolean is
   begin
      return (if not A then B else C); -- # ifnota
   end;

   function Ifnot1 (A, B, C : Boolean) return Boolean is
      Op : Op_Not_T;
   begin
      return (if Eval (Op, A) then B else C); -- # ifnota
   end;
end;
