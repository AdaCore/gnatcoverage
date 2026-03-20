package body Pck is
   procedure Break_Me (I : Integer) is
   begin
      null; --  COVERED
   end Break_Me;

   procedure Discard is
   begin
      null; --  ELIMINATED
   end Discard;

   procedure Break_Me (B : Boolean) is
   begin
      null;
   end Break_Me;
end Pck;
