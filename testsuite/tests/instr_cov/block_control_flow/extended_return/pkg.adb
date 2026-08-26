pragma Ada_2005;

package body Pkg is

   function Ext_Return (X : Integer) return Integer is
      T : Integer := X;
   begin
      return R : Integer := T do
         R := R + 1;
      end return;
      T := 0;
   end Ext_Return;

end Pkg;
