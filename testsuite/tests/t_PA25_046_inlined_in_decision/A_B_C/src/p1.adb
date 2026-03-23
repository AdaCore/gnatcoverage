with Twisters; use Twisters;

package body P1 is

   procedure Andthen (A, B, C : Boolean) is
   begin
      R := A and then Identity (B) and then C; -- # comb
   end;

end;
