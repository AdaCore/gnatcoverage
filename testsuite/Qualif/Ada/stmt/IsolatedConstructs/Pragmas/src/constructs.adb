with Support_Pragmas;
package body Constructs is

   subtype Acceptable_Integer is Integer range -20_000 .. 20_000;
   subtype Safe_Integer       is Integer range -10_000 .. 10_000;

   procedure Check_Val (I : in out Integer);
   pragma Precondition  (I in Acceptable_Integer, "wrong value"); -- # morethenoneinrange
   pragma Postcondition (I in Safe_Integer,       "failed to adjust value"); -- # morethenoneinrange

   procedure Check_Val (I : in out Integer) is
   begin
      if I not in Safe_Integer then                      -- # morethenoneinrange
         I := I / 2;                                     -- # neverexecuted
      end if;
   end Check_Val;

   function In_Range (X, L, R : Integer) return Boolean is
      Result : Boolean;
      Arg    : Integer;
   begin
      Arg := X;                                          -- # mainstream

      Result := True;                                    -- # mainstream

      pragma Debug (L > R, Support_Pragmas.Debug_Proc1); -- # mainstream

      if L > R then                                      -- # mainstream
         pragma Assert (Arg not in L .. R);              -- # emptyrange
         return False;                                   -- # emptyrange
      end if;


      if L = R then                                      -- # nonemptyrange
         pragma Debug (Support_Pragmas.Debug_Proc2);     -- # oneelement
         return Arg = L;                                 -- # oneelement
      end if;

      if Arg > R then                                    -- # morethenoneinrange
         Result := False;                                -- # XgtR
         pragma Assert (L < R);                          -- # XgtR
      end if;

      if Arg < L then                                    -- # morethenoneinrange
         Result := False;                                -- # XltL
         pragma Assert (L < R);                          -- # XltL
      end if;

      Check_Val (Arg);                                  -- # morethenoneinrange

      pragma Assert (Result = (Arg in L .. R));          -- # morethenoneinrange

      return Result;                                     -- # morethenoneinrange
   end In_Range;
end Constructs;
