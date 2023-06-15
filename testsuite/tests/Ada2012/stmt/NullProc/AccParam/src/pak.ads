pragma Ada_2012;

package Pak is
   procedure P2 (X : access Integer) is null; -- # npb
   procedure Call;
end Pak;
