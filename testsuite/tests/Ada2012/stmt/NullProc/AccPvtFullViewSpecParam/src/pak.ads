pragma Ada_2012;

package Pak is
   type R is abstract tagged limited null record;
   type PR is new R with private;
   procedure PPR1 (X : access PR);

   procedure Call;

private
   type PR is new R with null record;
   procedure PPR1 (X : access PR) is null; -- # npb
end Pak;

