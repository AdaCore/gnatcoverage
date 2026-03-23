pragma Assertion_Policy (Check);

with Ada.Text_IO;    use Ada.Text_IO;
with Silent_Last_Chance;

package body Pkg is

   procedure Mystery (A, B, C : Boolean)
   is
      function Id (B : Boolean) return Boolean;

      function Id (B : Boolean) return Boolean is
      begin
         return B;
      end Id;
   begin
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

         pragma Assert (Id (A or else B) or else A or else B);
         pragma Assert (B or else Id (A or else B));
      exception
         when others => null;
      end;
   end Mystery;

   procedure Other_Proc (A, B : Boolean) is
   begin
      begin
         if A and then B then
            Put_Line ("Stuff");
         else
            Put_Line ("not Stuff");
         end if;

         pragma Assert ((A and then B) or else A);
      exception
         when others => null;
      end;
   end Other_Proc;
end Pkg;
