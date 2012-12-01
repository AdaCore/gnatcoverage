package body FUAND is

   function Poslen_And_Eql (Ops : Keys) return Boolean is
      Myops : Keys renames Ops; -- # decl
      -- prevent 2 occurrences of "Op" on # evalB
   begin
      return Ops.A'Length > 0 -- # evalA
        and then Ops.A'Length = Myops.B'Length; -- # evalB
   end;
end;
