with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   I : Integer := 0;
begin
   pragma Annotate (Xcov, Exempt_Branch, "J");
   if I = 0 then
      Put_Line ("Null");
      pragma Annotate (Xcov, Exempt_Branch, "J");
   end if;
   pragma Annotate (Xcov, Exempt_Branch, "J");

   I := 10;

   pragma Annotate (Xcov, Exempt_Branch, "J");
   case I is
      pragma Annotate (Xcov, Exempt_Branch, "J");
      when 0 .. 100 =>
         Put_Line ("Small enough");
         pragma Annotate (Xcov, Exempt_Branch, "J");
         Put_Line ("Yet not negative");
         pragma Annotate (Xcov, Exempt_Branch, "J");

      when others =>
         null;
         pragma Annotate (Xcov, Exempt_Branch, "J");
   end case;
   pragma Annotate (Xcov, Exempt_Branch, "J");
end Main;
