pragma Ada_2012;

package Pkg is

   type Put_Char_Access is
     access procedure (C : Character);
   pragma Convention (C, Put_Char_Access);

   procedure Put_Char (C : Character);
   pragma Export (C, Put_Char, "pkg_put_char");

end Pkg;
