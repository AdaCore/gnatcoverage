pragma Ada_2012;

package Pak is
   type R is tagged limited null record;
   procedure PR3 (X : not null access R) is null; -- # npb
   procedure Call;
end Pak;
