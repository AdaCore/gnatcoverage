package body GOTO_Statements_Straight is

   function Map (N : Integer) return Integer is
      Result : Integer; -- # decl
   begin
      if N > 0 then                           -- # 1if
         goto GT_O;                           -- # 1goto
      end if;

      if N = 0 then                           -- # 2if
         goto EQ_0;                           -- # 2goto
      end if;

      Result := -N;                           -- # after2goto
      goto Fin;                               -- # 3goto

      <<GT_O>> Result := N ** 2;              -- # after3goto
      goto Fin;                               -- # 4goto

      <<EQ_0>> Result := 1;                   -- # after4goto

      <<Fin>> return Result;                  -- # fin
   end;

end GOTO_Statements_Straight;

