separate (Pack.Inner)
function Fun (I : Integer) return Integer is
   Res : Integer := I;             -- # stmt

   procedure Proc (I : in out Integer);
   procedure Proc (I : in out Integer) is separate;
begin
   if Res > 0 then                 -- # stmt
      Res := Res + 1;              -- # if
   else
      Proc (Res);                  -- # else
   end if;

   return Res;                     -- # stmt
end Fun;
