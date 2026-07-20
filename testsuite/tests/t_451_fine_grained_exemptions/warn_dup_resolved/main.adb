with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure Print_If (C1, C2 : Boolean; Message : String) is
   begin
      pragma Annotate (Xcov, Exempt_Decision_Outcome, False, "always true");
      pragma Annotate (Xcov, Exempt_Decision_Outcome, False, "never false");
      if C1 and then C2 then
         Put_Line (Message);
      end if;
   end Print_If;
begin
   Print_If (True, True, "some message");
end Main;
