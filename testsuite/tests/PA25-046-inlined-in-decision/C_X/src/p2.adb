package body P2 is
   
   function F2 (X : Integer) return Boolean;
   pragma Inline (F2);
   
   function F2 (X : Integer) return Boolean is
   begin
      return CX + X > 10;  -- # comp
   end;
   
   procedure Combine (C : Boolean; X : Integer) is
   begin
      R := C or else F2 (X); -- # or
   end;
end;
