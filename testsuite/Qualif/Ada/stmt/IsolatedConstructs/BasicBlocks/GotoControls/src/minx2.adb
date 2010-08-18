with Support; use Support;

function MinX2 (X : Natural) return Natural is
   M : Integer;
begin
   
   if X > 2 then   -- # Call
      goto M_Is_2; -- # MinIs2
   end if;
   
<<M_Is_X>>
    M := X;     -- # MinIsX
    goto Done;  -- # MinIsX
    
<<M_Is_2>>
    M := 2;     -- # MinIs2
    
<<Done>>
    Assert (M = Natural'Min (X, 2));  -- # Call
    return M;                         -- # Call
end;

