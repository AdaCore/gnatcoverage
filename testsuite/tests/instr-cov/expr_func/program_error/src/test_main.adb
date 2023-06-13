pragma Ada_2012;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;


procedure Test_Main is
   package File_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Unbounded_String);

   type R is record
      Source_Files : File_Sets.Set;
   end record;

   function It
     (Self : R)
      return File_Sets.Set_Iterator_Interfaces.Reversible_Iterator'Class
   is (Self.Source_Files.Iterate); -- # stmt

   Files : R;
begin
   Files.Source_Files.Include (To_Unbounded_String ("A")); -- # stmt
   for File in It (Files) loop                             -- # stmt
      Put_Line (To_String (File_Sets.Element (File)));     -- # stmt
   end loop;
end Test_Main;

--# test_main.adb
--
-- /stmt/ l+ ## 0
