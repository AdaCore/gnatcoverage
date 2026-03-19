pragma Ada_2012;

procedure Main is
   type Enum_Type is (A, B, C, D);
   type Flags_Type is array (Enum_Type) of Boolean;

   type Options_Type is record
      A, B, C : Boolean;
   end record;

   function Get_Flags (B : Boolean) return Flags_Type is
   (others => B);
   function Get_Some_Flags (B : Boolean) return Flags_type is
   (A => True, Others => B);

   function Get_Options (B : Boolean) return Options_Type is
   (others => B);
   function Get_Some_Options (B : Boolean) return Options_type is
   (A => True, Others => B);

   F1 : Flags_Type := Get_Flags (True);
   F2 : Flags_Type := Get_Some_Flags (True);
   O1 : Options_Type := Get_Options (True);
   O2 : Options_Type := Get_Some_Options (True);
begin
   null;
end Main;
