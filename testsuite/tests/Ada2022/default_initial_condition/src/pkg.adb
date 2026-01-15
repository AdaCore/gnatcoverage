pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   procedure Set_Defaults (X, Y : Integer) is
   begin
      Default_X := X;
      Default_Y := Y;
   end Set_Defaults;

   ------------
   -- Create --
   ------------

   function Create (X, Y : Integer) return Point is
   begin
      return (X, Y);
   end Create;

   -----------
   -- Get_X --
   -----------

   function Get_X (Self : Point) return Integer is
   begin
      return Self.X;
   end Get_X;

   -----------
   -- Get_Y --
   -----------

   function Get_Y (Self : Point) return Integer is
   begin
      return Self.Y;
   end Get_Y;

   -----------
   -- Print --
   -----------

   procedure Print (Self : Point) is
   begin
      Put_Line ("Point(" & Self.X'Image & ", " & Self.Y'Image & ")");
   end Print;

end Pkg;
