with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C;

procedure Main is
   Input : constant String := "abc def ghi";

   function Get_Number (I, Negate : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Get_Number, "get_number");

   Word_Started : Boolean := False;
   Word_Count   : Natural := 0;
begin
   for I in Input'Range loop
      case Input (I) is
         when ' ' =>
            Word_Started := False;
         when 'a' .. 'z' | 'A' .. 'Z' =>
            if not Word_Started then
               Word_Started := True;
               Word_Count := Word_Count + Integer (Get_Number (1, 0));
            end if;
         when others =>
            raise Program_Error;
      end case;
   end loop;

   if Word_Count /= 3 then
      raise Program_Error;
   end if;
end Main;
