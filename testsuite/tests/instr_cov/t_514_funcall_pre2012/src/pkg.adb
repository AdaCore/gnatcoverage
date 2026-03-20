pragma Ada_2005;

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   --------------
   -- Identity --
   --------------

   function Identity (B : Boolean) return Boolean is
   begin
      return B;                                   -- # stmt
   end Identity;

   -----------
   -- Print --
   -----------

   procedure Print (B : Boolean) is
      Actual : constant Boolean := Identity (B);  -- # call
   begin
      if Actual then                              -- # stmt
         Put_Line ("True");                       -- # stmt
      else
         Put_Line ("False");                      -- # stmt
      end if;
   end Print;

end Pkg;
