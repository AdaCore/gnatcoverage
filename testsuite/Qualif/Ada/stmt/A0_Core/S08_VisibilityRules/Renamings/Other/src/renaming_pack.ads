with Pack; use Pack;
package Renaming_Pack is
   procedure Q3 (I : in out Integer) renames P3;

   function R2 (I : Integer := 1) return Integer renames F2;
   function R3 (I : Integer) return Integer renames F3;
end Renaming_Pack;
