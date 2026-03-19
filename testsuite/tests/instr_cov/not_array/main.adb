pragma Ada_2012;

procedure Main is
   type Enum_Type is (A, B, C, D);
   type Flags_Type is array (Enum_Type) of Boolean;

   function Identity (B : Boolean) return Boolean is (B);
   function Get_Flags (B : Boolean) return Flags_Type
   is (Flags_Type'(others => B));

   X     : Boolean := Identity (True);
   Y     : Boolean := Identity (False);
   Flags : Flags_Type := not Get_Flags (X and then Y);
begin
   Flags := not Flags;
end Main;
