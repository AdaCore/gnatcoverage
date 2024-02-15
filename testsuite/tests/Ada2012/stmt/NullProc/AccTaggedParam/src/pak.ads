pragma Ada_2012;

package Pak is
   type R is tagged limited null record;
   procedure PR2 (X : access R) is null; -- # npb
   procedure Call;
end Pak;
