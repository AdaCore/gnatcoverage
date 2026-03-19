with Doraise; use Doraise;

package Ctxt is

   -- A few globals that let us insert statements at various
   -- spots and sanity check the logic we ran through.

   X : Integer := 0;
   Handled : Integer := 0;

   procedure Run (Rk : Raise_Kind; Rm : Raise_Mode);
   -- Invoke and optionally handle Doraise operations in a specific
   -- context. Body to be provided by the various testcases, each
   -- exercising a particular situation.
end;
