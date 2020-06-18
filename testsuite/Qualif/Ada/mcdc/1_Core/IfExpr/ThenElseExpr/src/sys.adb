pragma Ada_2012;

with Ops; use Ops;

package body Sys is
   function And_Or_C0 (Cond, A, B, C : Boolean) return Boolean is
      Self: Op_Self_T;
   begin
      return (if Self.Eval(Cond) then (A and then B) else C); -- # eval
   end;

   function C_Or_Or0 (Cond, A, B, C : Boolean) return Boolean is
      Self: Op_Self_T;
   begin
      return (if Self.Eval(Cond) then C else (A or else B)); -- # eval
   end;

end;
