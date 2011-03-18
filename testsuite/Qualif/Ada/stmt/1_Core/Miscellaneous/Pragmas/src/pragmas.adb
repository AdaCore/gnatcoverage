pragma Check_Policy (Assertion, On);
pragma Check_Policy (Precondition, On);
pragma Check_Policy (Postcondition, On);

with Support_Pragmas;

package body Pragmas is

   procedure Check_Val (I : in out Integer);
   pragma Precondition  (I in Acceptable_Integer, "wrong value"); -- # pre_check_val
   pragma Postcondition (I in Safe_Integer,       "failed to adjust value"); -- # post_check_val

   procedure Check_Val (I : in out Integer) is
   begin
      if I not in Safe_Integer then                      -- # check_val
         I := I / 2;                                     -- # neverexecuted
      end if;
   end Check_Val;

   function In_Range (X, L, R : Integer) return Boolean is
      Result : Boolean;
      Arg    : Integer;
   begin
      Arg := X;                                          -- # mainstream

      Result := True;                                    -- # mainstream

      pragma Debug (L > R, Support_Pragmas.Debug_Proc1); -- # 1debug

      if L > R then                                      -- # mainstream
         pragma Assert (Arg not in L .. R);              -- # 1assert
         return False;                                   -- # emptyrange
      end if;


      if L = R then                                      -- # nonemptyrange
         pragma Debug (Support_Pragmas.Debug_Proc2);     -- # 2debug
         return Arg = L;                                 -- # oneelement
      end if;

      if Arg > R then                                    -- # morethenoneinrange
         Result := False;                                -- # XgtR
         pragma Assert (L < R);                          -- # 2assert
      end if;

      if Arg < L then                                    -- # morethenoneinrange
         Result := False;                                -- # XltL
         pragma Assert (L < R);                          -- # 3assert
      end if;

      pragma Assert (Result = (Arg in L .. R));          -- # 4assert

      return Result;                                     -- # morethenoneinrange
   end In_Range;

   function Is_Safe (I : Integer) return Boolean is
      Tmp : Integer := I;                                -- # is_safe
   begin
      Check_Val (Tmp);                                   -- # is_safe
      return Tmp = I;                                    -- # is_safe
   end Is_Safe;

end Pragmas;
