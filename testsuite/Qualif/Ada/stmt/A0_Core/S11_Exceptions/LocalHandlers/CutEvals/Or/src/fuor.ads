package Fuor is
   procedure Orelse (A, B : Integer; R : out Boolean);

   -- Constants that map to False, True or raise, respectively

   F : constant := 0;
   T : constant := 1;
   R : constant := -1;
end;
