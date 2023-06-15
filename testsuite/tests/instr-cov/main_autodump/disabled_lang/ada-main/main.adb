with Interfaces.C; use Interfaces.C;

procedure Main is
   function fact (N : int) return int;
   pragma Import (C, fact);
begin
   if fact (1) /= 1 then
      raise Program_Error;
   end if;
end Main;
