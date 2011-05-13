package Fuor is
   function Orelse (A, B : Integer) return Boolean;

   -- Constants that map to False, True or raise, respectively

   F : constant := 0;
   T : constant := 1;
   R : constant := -1;
end;
