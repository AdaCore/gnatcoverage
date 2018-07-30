package body LOOP_Statements is

   function All_Less_Then (L : Level; S  : Sample) return Boolean is
      Last_LT : Boolean; -- # declLoop1
      -- Whether the last examined sample value is known to be < L
   begin

      --  Start True, value to return if S is empty, then loop over S elements
      --  as long as none is found not to satisfy the criterion.

      Last_LT := True;              -- # preLoop1

      for I in S'Range loop         -- # loop1
         Last_LT := False;          -- # inloopbeforeexit1
         exit when not (S (I) < L); -- # inloopexit1
         Last_LT := True;           -- # inloopafterexit1
      end loop;

      return Last_LT;               -- # postLoop1
   end;

   procedure Find_Last_Char
     (Res : out Natural;
      Str :     String;
      Ch  :     Character)
   is
      First : Natural; -- # declLoop2
      Last  : Natural; -- # declLoop2
   begin
      First := Str'First;                  -- # preLoop2
      Last  := Str'Last;                   -- # preLoop2
      Res   := 0;                          -- # preLoop2

      for J in reverse First .. Last loop  -- # loop2
         if Str (J) = Ch then              -- # inloopbeforeexit2
            Res := J;                      -- # inloopbeforeexitinif2
            exit;                          -- # inloopexit2
         end if;
      end loop;

   end Find_Last_Char;

   function Factorial (N : T) return T is
      Res   : T; -- # declLoop3
      Count : T; -- # declLoop3
   begin
      Res   := 1;                   -- # preLoop3
      Count := 1;                   -- # preLoop3

      loop
         exit when Count > N;       -- # inloopexit3
         Res   := Res * Count;      -- # inloop3
         Count := Count + 1;        -- # inloop3
      end loop;

      return Res;                   -- # postLoop3
   end Factorial;

   procedure Sum_First_Under_Limit
      (Res : out Integer;
       Arg :     Big_Sample)
   is
      Idx   : Natural; -- # declLoop4
      First : Natural; -- # declLoop4
      Last  : Natural; -- # declLoop4
   begin
      Res   := 0;                                -- # preLoop4
      First := Arg'First;                        -- # preLoop4
      Last  := Arg'Last;                         -- # preLoop4
      Idx   := First;                            -- # preLoop4

      while Idx <= Last loop                     -- # loop4
         exit when Arg (Idx) >= Integer (Limit); -- # inloopexit4
         Res := Res + Arg (Idx);                 -- # inloop4
         Idx := Idx + 1;                         -- # inloop4
      end loop;

   end Sum_First_Under_Limit;

end LOOP_Statements;

