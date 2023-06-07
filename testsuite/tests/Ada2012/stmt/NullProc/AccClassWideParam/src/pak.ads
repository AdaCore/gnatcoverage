pragma Ada_2012;

package Pak is
   type R is tagged limited null record;
   procedure PR4 (X : access R'Class) is null; -- # npb

   procedure Call;
end Pak;
