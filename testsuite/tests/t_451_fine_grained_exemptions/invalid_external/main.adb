with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   procedure Print_If (Condition : Boolean; Message : String) is
   begin
      if Condition then
         Put_Line (Message);
      end if;
   end Print_If;

begin
   Print_If (True, "message A1");
end Main;
