with Ada.Text_IO; use Ada.Text_IO;

procedure Proc (N : Natural) is
begin
   if N = 1 then
      Put_Line ("got one argument");
   elsif N in 2 .. 10 then
      pragma Annotate (Xcov, Exempt_On, "2 .. 10");
      Put_Line ("got between 2 and 10 arguments");
      pragma Annotate (Xcov, Exempt_Off);
   else
      pragma Annotate (Xcov, Exempt_On, "others");
      Put_Line ("other cases");
      pragma Annotate (Xcov, Exempt_Off);
   end if;
end Proc;
