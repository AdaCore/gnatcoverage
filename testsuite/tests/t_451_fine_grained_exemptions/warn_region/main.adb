with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure Print_If (B : Boolean; Message : String) is
   begin
      pragma Annotate (Xcov, Exempt_Decision_Outcome, False, "J2");
      if Boolean'(B) then
         Put_Line (Message);
      end if;
   end Print_If;
begin
   Print_If (True, "some message");
end Main;
