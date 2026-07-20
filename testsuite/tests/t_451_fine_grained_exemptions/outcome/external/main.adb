pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   procedure Print_If (Condition : Boolean; Message : String) is
   begin
      if Condition then
         Put_Line (Message);
      end if;
   end Print_If;

   procedure Print_If_2 (C1, C2 : Boolean; Message : String) is
   begin
      if C1 and then Boolean'
                       (if C2 then True else False)
      then
         Put_Line (Message);
      end if;
   end Print_If_2;

begin
   Print_If (True, "message A1");

   Print_If_2 (False, False, "message B1");
   Print_If_2 (True, False, "message B2");
   Print_If_2 (True, True, "message B3");
end Main;
