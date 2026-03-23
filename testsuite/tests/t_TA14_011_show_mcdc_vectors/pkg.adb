with Ada.Text_IO; use Ada.Text_IO;
package body Pkg is

   procedure Mystery (A, B, C : Boolean) is
   begin
      if (A and then B and then C) or else A then
         Put_Line ("Great success!");
      else
         Put_Line ("Failed");
      end if;
      if A then
         Put_Line ("A is True");
      else
         Put_Line ("A is False");
      end if;
   end Mystery;

   procedure Other_Proc (A, B : Boolean) is
   begin
      if A and then B then
         Put_Line ("Stuff");
      else
         Put_Line ("not Stuff");
      end if;
   end Other_Proc;
end Pkg;
