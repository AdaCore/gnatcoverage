package body Text_Io is

   procedure Flush is
   begin
      null;
   end;

   procedure New_Line is
   begin
      Put (ASCII.LF);
   end New_Line;

   procedure Put (Item : in Character) is
      procedure Internal (C   : Character);
      pragma Import (C, Internal, "putchar");
   begin
      Internal (Item);
   end Put;

   procedure Put (Item : in String) is
   begin
      for I in Item'Range loop
         Put (Item (I));
      end loop;
   end Put;

   procedure Put_Line (Item : in String) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;

   procedure Get (Item : out Character) is
      function Internal return Character;
      pragma Import (C, Internal, "getchar");
   begin
      Item := Internal;
   end Get;
end Text_Io;
