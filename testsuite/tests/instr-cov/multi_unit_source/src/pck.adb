with U1;
with U2;
with U3;

package body Pck is

   procedure Call is
   begin
      U2.Increment;
      U1.A := U1.A + U2.B;
      U3;
   end Call;

end Pck;
