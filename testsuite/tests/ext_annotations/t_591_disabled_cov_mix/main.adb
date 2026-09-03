with Ada.Text_IO;  use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;

procedure Main is
   function Is_Even (I : int) return int;
   pragma Import (C, Is_Even, "is_even");

   --  Placeholder for external annotations

   --  Do not include anything coverable in the disabled coverage region, so
   --  that only source contents and disabled regions differ.

   pragma Annotate (Xcov, Cov_Off, "test");  -- REMOVEME
   pragma Annotate (Xcov, Cov_On);  -- REMOVEME
begin
   if Is_Even (2) /= 0 then
      Put_Line ("Hello world");
   end if;
end Main;
