package body EXIT_Statements is

   function Factorial (N : T) return T is
      Res   : T;
      Count : T;
   begin
      Res   := 1;                        -- # 1preloop
      Count := N;                        -- # 1preloop

      loop                               -- # 1loop
         exit when Count <= 0;           -- # in1loop1exit
         exit when Res > T'Last / Count; -- # in1loop2exit
         Res   := Res * Count;           -- # in1loopafterexit
         Count := Count - 1;             -- # in1loopafterexit
      end loop;

      return Res;                        -- # post1loop
   end Factorial;

   procedure Update_Array_Sample
     (Arg   : in out Array_Sample;
      Limit :        Positive)
   is
      Sum : Integer;
   begin
      Sum := 0;                          -- # 2preloop

      for J in Arg'Range loop            -- # 2loop
         exit when Arg (J) < 0;          -- # 21exit
         Sum := Sum + Arg (J);           -- # 2after1exit

         if Sum > Limit then             -- # 2after1exit
            exit;                        -- # 22exit
         end if;

         Arg (J) := Sum;                 -- # 2after2exit

      end loop;

   end Update_Array_Sample;

   function Compute_On_Matrix
     (M     : Matrix_Sample;
      Limit : Natural)
      return  Natural
   is
      Result     : Natural;

      First_Line : Natural;
      Last_Line  : Natural;
      Line_Idx   : Natural;

      First_Col     : Natural;
      Last_Col   : Natural;
      Col_Idx    : Natural;

   begin
      Result := 0;                                     -- # 3preloop

      First_Line := M'First (1);                       -- # 3preloop
      Last_Line  := M'Last (1);                        -- # 3preloop
      Line_Idx   := First_Line;                        -- # 3preloop

      First_Col  := M'First (2);                       -- # 3preloop
      Last_Col   := M'Last (2);                        -- # 3preloop
      Col_Idx    := First_Col;                         -- # 3preloop

      Outer : while Line_Idx <= Last_Line loop         -- # 3outerloop

         Inner : while Col_Idx <= Last_Col loop        -- # 3innerloop
            exit Outer when M (Line_Idx, Col_Idx) < 0; -- # 31exit

            Result := Result + M (Line_Idx, Col_Idx);  -- # 3after1exit

            if Result > Limit then                     -- # 3after1exit
               exit Outer;                             -- # 32exit
            end if;

            Col_Idx := Col_Idx + 1;                    -- # 3after2exit
         end loop Inner;

         Line_Idx := Line_Idx + 1;                     -- # 3afterinnerloop
         Col_Idx  := First_Col;                        -- # 3afterinnerloop
      end loop Outer;

      return Result;                                   -- # postloop

   end Compute_On_Matrix;

end EXIT_Statements;

