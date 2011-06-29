package body Block_Statements is

   procedure Swap_G (L, R : in out T) is
   begin

      if L /= R then            -- # 1if
         declare
            Tmp : T := L;       -- # 1dclblock
         begin
            L := R;             -- # 1stmtblock
            R := Tmp;           -- # 1stmtblock
         end;
      end if;

   end Swap_G;

   procedure Swap_Max_Min (Arg : in out Sample) is
   begin
      if Arg'Length >= 2 then                          -- # 2if
         declare
            Max_Idx : Natural := Arg'First;            -- # 2dclblock
            Min_Idx : Natural := Arg'First;            -- # 2dclblock

            Max : Integer := Arg (Max_Idx);            -- # 2dclblock
            Min : Integer := Arg (Min_Idx);            -- # 2dclblock
         begin
            for J in Arg'First + 1 .. Arg'Last loop    -- # 2stmtblock
               if Arg (J) < Min then                   -- # 21ifstmtblock
                  Min     := Arg (J);                  -- # 2in1ifstmtblock
                  Min_Idx := J;                        -- # 2in1ifstmtblock
               end if;

               if Arg (J) > Max then                   -- # 22ifstmtblock
                  Max     := Arg (J);                  -- # 2in2ifstmtblock
                  Max_Idx := J;                        -- # 2in2ifstmtblock
               end if;
            end loop;

            Arg (Min_Idx) := Max;                      -- # 2stmtblock
            Arg (Max_Idx) := Min;                      -- # 2stmtblock
         end;
      end if;

   end Swap_Max_Min;

   function Factorial (N : T) return T is
      Result : T;

      --  To make sure that we get a check within the loop and that the
      --  handler remains around

      pragma Unsuppress (Overflow_Check);

   begin
      if N < 0 then                                    -- # 3stmt
         Result := 0;                                  -- # 3inif
      else
         begin
            Result := 1;                               -- # 3blockstmt

            for J in 2 .. N loop                       -- # 3loopstmtblock
               Result := Result * J;                   -- # 3inloopstmtblock
            end loop;
         exception
            when Constraint_Error =>
               Result := T'Last;                       -- # 3handlerblock
         end;
      end if;

      return Result;                                   -- # 3stmt
   end Factorial;

   function Sum_Min_Max (Arg : Sample) return Integer is
      Result : Integer;
   begin
      if Arg'Length = 0 then                           -- # 4stmt
         Result := 0;                                  -- # 4inif
      else
         declare
            Max : Integer := Arg (Arg'First);          -- # 4dclblock
            Min : Integer := Arg (Arg'First);          -- # 4dclblock
         begin
            for J in Arg'First + 1 .. Arg'Last loop    -- # 4blockstmt
               if Arg (J) < Min then                   -- # 41ifstmtblock
                  Min := Arg (J);                      -- # 4in1ifstmtblock
               end if;

               if Arg (J) > Max then                   -- # 41ifstmtblock
                  Max := Arg (J);                      -- # 4in2ifstmtblock
               end if;
            end loop;

            Result := Min + Max;                       -- # 4blockstmt
         end;
      end if;

      return Result;                                   -- # 4stmt
   end Sum_Min_Max;

end Block_Statements;

