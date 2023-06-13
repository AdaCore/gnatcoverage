--  Tests override this unit to implement testcases

with Ada.Text_IO; use Ada.Text_IO;

separate (Pkg)
procedure Test is
begin
   Put_Line ("!!! Test stub called in production setup");
   raise Program_Error;
end Test;
