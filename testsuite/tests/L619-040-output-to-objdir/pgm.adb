procedure PGM is

   --  This is GPR specific test for which we produce our own local project,
   --  without any libsupport consideration. We must make sure that we don't
   --  need a libsupport at all:
   
   pragma Suppress (All_Checks);

   X : Integer := 12;
   pragma Volatile (X);
begin
   X := X + 1;
end;
