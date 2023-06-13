pragma Ada_2012;

package Pak is
   procedure P2
     (X : access function (S : access String) return Natural)
   is null; -- # npb
   procedure Call;
end Pak;
