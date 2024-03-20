with Utils; use Utils;

separate (Pkg)
procedure Proc is

   package Nested is
      procedure Set (Value : Integer);
      function Get return Integer;
   end Nested;

   package body Nested is separate;

begin
   Nested.Set (1);
   if Identity (Nested.Get) /= 1 then
      raise Program_Error;
   end if;
end Proc;
