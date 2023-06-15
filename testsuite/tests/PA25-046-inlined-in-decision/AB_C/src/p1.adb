package body P1 is
   
   function F1 (A, B : Boolean) return Boolean;
   pragma Inline (F1);
   
   function F1 (A, B : Boolean) return Boolean is
   begin
      return A and then B; -- # and
   end;
   
   procedure Combine (A, B, C : Boolean) is
   begin
      R := F1 (A, B) or else C; -- # or
   end;
   
end;
