package body Decls_Pack is

   function Simple_Sort (Arg : Vector) return Vector is
      Result      : Vector  := Arg;                    -- # local_dcl
      First       : Integer := Result'First;           -- # local_dcl
      Last        : Integer := Result'Last;            -- # local_dcl
      Current_Max : Integer := First;                  -- # local_dcl
      Tmp         : Integer;
   begin
      for J in First .. Last loop                      -- # stmt
         Current_Max := J;                             -- # stmt

         for K in J .. Last loop                       -- # stmt
            if Result (K) > Result (Current_Max) then            -- # stmt
               Current_Max := K;                       -- # stmt
            end if;
         end loop;

         Tmp                  := Result (J);           -- # stmt
         Result (J)           := Result (Current_Max); -- # stmt
         Result (Current_Max) := Tmp;                  -- # stmt

      end loop;

      return Result;                                   -- # stmt
   end Simple_Sort;

end Decls_Pack;
