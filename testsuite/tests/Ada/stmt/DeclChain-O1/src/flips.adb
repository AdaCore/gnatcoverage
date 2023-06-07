package body Flips is

   Ob, T : Boolean := True;
   pragma Volatile (T);

   procedure Flip (B : in out Boolean) is
   begin
      B := not B;  -- # doflip
   end;

   procedure Flip_If (P : Boolean) is
      KB : constant Boolean := T;
      B : Boolean := KB;
   begin
      if P then
         Flip (B); -- # doflip
         Ob := B;  -- # doflip
      end if;
   end;
end;
