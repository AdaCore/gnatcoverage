package body FUOR is

   function Empty_Or_Eql (Ops : Keys) return Boolean is
      Myops : Keys renames Ops; -- prevent 2 occurrences of "Op" on # evalB
   begin
      return Ops.A'Length = 0 -- # evalA
        or else Ops.A'Length = Myops.B'Length; -- # evalB
   end;
end;
