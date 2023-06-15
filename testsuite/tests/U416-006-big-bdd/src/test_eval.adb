with Ada.Text_IO;

with Interfaces.C; use Interfaces.C;

with TestConditions;

procedure Test_Eval is
   function C_Compute (C1, C2, C3, C4, C5, C6, C7, C8 : int) return int;
   pragma Import (C, C_Compute);

   Dummy : int;
begin
   Ada.Text_IO.Put_Line ("Hello_World");
   TestConditions.Run;

   --  Exercize both outcomes so that conditions are labeled with undetermined
   --  coverage.

   Dummy := C_Compute (0, 0, 0, 0, 0, 0, 0, 0);
   Dummy := C_Compute (0, 0, 0, 0, 0, 0, 0, 1);
end Test_Eval;
