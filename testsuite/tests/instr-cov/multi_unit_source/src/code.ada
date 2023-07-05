package U1 is
   A : Integer := 1;
end U1;

package U2 is
   B : Integer := 2;
   procedure Increment;
   procedure Decrement;
end U2;

package body U2 is
   procedure Increment is
   begin
      B := B + 1;
   end Increment;
   procedure Decrement is
   begin
      B := B - 1;
   end Decrement;
end U2;

with U1, U2;
procedure U3 is
begin
   U2.Increment;
   U1.A := U1.A + U2.B;
   U2.Decrement;
end U3;
