pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   procedure Print_If (Message : String; C1, C2, C3 : Boolean) is
   begin
      if C1 and then Boolean'
                       (if C2 and then C3 then True else False)
      then
         Put_Line (Message);
      end if;
   end Print_If;

   function F return Boolean renames False;
   function T return Boolean renames True;

begin
   Print_If ("message B1", F, T, T);
   Print_If ("message B2", T, F, F);
   Print_If ("message B3", T, T, T);
end Main;
