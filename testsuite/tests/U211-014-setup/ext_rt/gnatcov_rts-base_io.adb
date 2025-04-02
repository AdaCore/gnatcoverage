with GNAT.IO;

package body GNATcov_RTS.Base_Io is

   Printed_Tag : Boolean := False;

   function Dummy_Predicate return Boolean;
   procedure Print_Tag;

   ---------------------
   -- Dummy_Predicate --
   ---------------------

   function Dummy_Predicate return Boolean is
   begin
      GNAT.IO.Put_Line ("<PREDICATE>");
      GNAT.IO.New_Line;
      return True;
   end Dummy_Predicate;

   ---------------
   -- Print_Tag --
   ---------------

   procedure Print_Tag is
   begin
      if not Printed_Tag then
         Printed_Tag := True;
         pragma Assert (Dummy_Predicate);
         GNAT.IO.Put_Line ("<TAG>");
         GNAT.IO.New_Line;
      end if;
   end Print_Tag;

   ---------
   -- Put --
   ---------

   procedure Put (S : GNATcov_RTS_String) is
      pragma Warnings (Off);
      Str : String (1 .. Integer (S.Length));
      for Str'Address use S.Str;
      pragma Warning (On);
   begin
      Print_Tag;
      GNAT.IO.Put_Line (Str);
   end Put;

end GNATcov_RTS.Base_Io;
