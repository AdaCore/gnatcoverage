with Silent_Last_Chance;
with Doraise; use Doraise;

package body Ctxt is
   procedure Run (Rk : Raise_Kind; Rm : Raise_Mode) is
   begin
      X := X + 1; -- # pre
      Dispatch (Rk, Rm); -- # run
      X := X + 1; -- # post
   end;
end;
