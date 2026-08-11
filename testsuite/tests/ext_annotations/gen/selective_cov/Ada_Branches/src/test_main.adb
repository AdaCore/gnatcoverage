--  Check that external disabled coverage regions around branching statements
--  are properly taken into account by the Ada instrumenter.

procedure Test_Main is

   procedure Foo (X : Boolean) is null;  -- # foo

   --  Whole IF statement, ELSIF and ELSE parts included, in a disabled
   --  coverage region.

   procedure All_Disabled (A, B : Boolean) is
   begin
      if A and then B then               -- # all_off
         Foo (A);                        -- # all_off
      elsif A or else B then             -- # all_off
         Foo (B);                        -- # all_off
      else                               -- # all_off
         Foo (A);                        -- # all_off
      end if;                            -- # all_off
   end All_Disabled;

   --  Only the ELSE part is in a disabled coverage region: the decision
   --  controlling the IF statement is still instrumented.

   procedure Else_Disabled (A, B : Boolean) is
   begin
      if A and then B then               -- # else_on
         Foo (A);                        -- # else_on
      else                               -- # else_off
         Foo (B);                        -- # else_off
      end if;                            -- # else_off
   end Else_Disabled;

   --  Whole CASE statement in a disabled coverage region

   procedure Case_Disabled (V : Integer) is
   begin
      case V is                          -- # case_off
         when 1      => Foo (True);      -- # case_off
         when others => Foo (False);     -- # case_off
      end case;                          -- # case_off
   end Case_Disabled;

begin
   All_Disabled (True, True);            -- # main
   Else_Disabled (True, True);           -- # main
   Else_Disabled (True, False);          -- # main
   Else_Disabled (False, True);          -- # main
   Case_Disabled (1);                    -- # main
   Case_Disabled (2);                    -- # main
end Test_Main;

--# test_main.adb
--
-- /foo/      l+ ## 0
-- /main/     l+ ## 0
-- /all_off/  lD ## 0
-- /else_on/  l+ ## 0
-- /else_off/ lD ## 0
-- /case_off/ lD ## 0
