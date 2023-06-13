
package body Plop is

   function Identity (V : Value) return Value;

   function Steps_To_LT (X, LB : Value; Max : Step) return Step is
      V : Value := X;
      N : Step := 0;
   begin
      loop
         exit when abs(V) < LB or else N >= Max; -- # eval
         V := Identity (V) - 1.0;                -- # body
         N := N + 1;                             -- # body
      end loop;
      return N;
   end;

   function Identity (V : Value) return Value is
      Latch : Value := V;
      pragma Volatile (Latch);
   begin
      return Latch;
   end;
end;
