package body Pack is
   procedure Local (I : in out Integer) is
   begin
      I := I * 2;                     -- # local
   end Local;

   procedure P1 (I : in out Integer) is
      procedure P1_Local (J : in out Integer) renames P2; -- # p1decl
   begin
      I := I + 1;                     -- # p1
      P1_Local (I);                   -- # p1
   end P1;

   procedure P2 (I : in out Integer) is
   begin
      I := I + 2;                     -- # p2
   end P2;

   procedure P3 (I : in out Integer) is
   begin
      I := I + 3;                     -- # p3
   end P3;

   function F1 (I : Integer) return Integer is
      procedure F1_Local (X : in out Integer) renames Local; -- # declf1
      Res : Integer := I;                                    -- # declf1
   begin
      F1_Local (Res);                                        -- # f1
      return Res;                                            -- # f1
   end F1;

   function F2 (I : Integer) return Integer is
   begin
      return I + 2;                   -- # f2
   end F2;

   function F3 (I : Integer) return Integer is
   begin
      return I + 3;                   -- # f3
   end F3;
end Pack;
