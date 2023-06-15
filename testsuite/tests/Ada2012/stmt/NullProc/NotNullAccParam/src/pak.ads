pragma Ada_2012;

package Pak is
   procedure P3 (X : not null access Integer) is null; -- # npb
   procedure Call;
end Pak;
