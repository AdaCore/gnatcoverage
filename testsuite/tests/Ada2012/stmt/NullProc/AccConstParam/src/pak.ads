pragma Ada_2012;

package Pak is
   procedure P4 (X : access constant Integer) is null; -- # npb
   procedure Call;
end Pak;
