with Support; use Support;

package body Overloads is

   procedure Flip (X : in out Integer) is
   begin
      X := -X;     -- # flipx
   end;

   procedure Flip (B : in out Boolean) is
   begin
      B := not B;  -- # flipb
   end;

   procedure Dispatch (Flipx, Flipb : Boolean) is
      KX : constant Integer := Identity (12);
      KB : constant Boolean := Identity (True);
      B : Boolean := KB;
      X : Integer := KX;
   begin
      if Flipb then
         Flip (B);            -- # flipb
         Assert (B = not KB); -- # flipb
      end if;
      if Flipx then
         Flip (X);            -- # flipx
         Assert (X = -KX);    -- # flipx
      end if;
   end;
end;
