pragma Ada_2012;

package Pkg is
   procedure Foo (I : Integer) with Pre => I > 0;
   procedure Bar;

   I : Integer;
end Pkg;
