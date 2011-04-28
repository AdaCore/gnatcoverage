procedure Inc (X : in out Integer; Amount : Integer) is
begin
   if Amount /= 0 then -- # inc
      X := X + 1;      -- # inc
   end if;
end;
