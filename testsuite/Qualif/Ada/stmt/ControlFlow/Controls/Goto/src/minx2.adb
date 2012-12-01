with Support; use Support;

function MinX2 (X : Natural) return Natural is
   M : Integer; -- # decl
begin

   if X > 2 then   -- # common
      goto Xgt2; -- # xgt2
   end if;

<<Xle2>>
    M := X;     -- # xle2
    goto Done;  -- # xle2

<<Xgt2>>
    M := 2;     -- # xgt2

<<Done>>
    Assert (M = Natural'Min (X, 2));  -- # common
    return M;                         -- # common
end;

