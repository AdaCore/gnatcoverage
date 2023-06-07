pragma Ada_2012;

package Pak is
   type R is tagged limited null record;
   procedure PR5 (X : access constant R'Class) is null; -- # npb

   procedure Call;
end Pak;
