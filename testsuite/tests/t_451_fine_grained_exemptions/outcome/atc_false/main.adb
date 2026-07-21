pragma Ada_2012;
pragma Assertion_Policy (Check);

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   pragma Annotate (Xcov, Exempt_Decision_Outcome, False, "never false");
   procedure Print_If (C1, C2 : Boolean; Message : String)
   with Pre => C1 or else C2;

   procedure Print_If (C1, C2 : Boolean; Message : String) is
   begin
      if C1 and then C2 then
         Put_Line (Message);
      end iF;
   end Print_If;

begin
   Print_If (True, False, "some message");
end Main;
