pragma Ada_2012;

package Pkg is
   type T is (Boolean, String);
   function F (I : Integer) return Integer is (I + 1); -- # stmt
end Pkg;
