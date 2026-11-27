pragma Ada_2012;

package Pkg is
   procedure P is
      null
   with
      Export,
      Convention => C,
      External_Name => "main__p";
end Pkg;
