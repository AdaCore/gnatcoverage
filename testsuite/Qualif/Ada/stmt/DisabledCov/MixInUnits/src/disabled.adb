package body Disabled is

   procedure Swap (I, J : in out Integer) is

      --  Disable coverage for a local declaration

      pragma Annotate                                         -- # disabled
        (Xcov, Cov_Off, "disable coverage on declarations");  -- # disabled
      Tmp : Integer := J;                                     -- # disabled
      pragma Annotate (Xcov, Cov_On);                         -- # disabled
   begin
      J := I;                                              -- # swap_stmt
      I := Tmp;                                            -- # swap_stmt
   end Swap;

   function Factorial (X : Natural) return Positive is
   begin
      if X = 1 then                                        -- # factorial
         return 1;                                         -- # 1_factorial
      elsif X >= Positive'Last / 1000 then                 -- # elsif_factorial

         --  Disable coverage for a conditioned set of statements

         pragma Annotate                                   -- # disabled
           (Xcov, Cov_Off,                                 -- # disabled
            "disable coverage on statements in function"); -- # disabled
         return Positive'Last;                             -- # disabled
         pragma Annotate (Xcov, Cov_On);                   -- # disabled
      else
         return X * Factorial (X - 1);                     -- # rec_factorial
      end if;
   end Factorial;

   --  No disable coverage in the couple of subprograms below

   procedure Another_Swap (I, J : in out Integer) is
      Tmp : Integer := J;                                  -- # another_swap
   begin
      J := I;                                              -- # another_swap
      I := Tmp;                                            -- # another_swap
   end Another_Swap;

   function Another_Factorial (X : Natural) return Positive is
      Result : Positive := 1;                   -- # another_factorial
   begin
      for J in 1 .. X loop                      -- # another_factorial
         if Result > Positive'Last / J then     -- # in_loop_a_f
            raise Constraint_Error;             -- # in_if_a_f
         end if;

         Result := Result * J;                  -- # in_loop_a_f
      end loop;

      return Result;                            -- # another_factorial
   exception
      when Constraint_Error =>
         return Positive'Last;                  -- # handler_a_f
   end Another_Factorial;


begin

   --  Mix of disabled and non-disabled coverage regions for statements in the
   --  package elaboration sequence.

   pragma Annotate                                 -- # disabled
     (Xcov, Cov_Off,                               -- # disabled
      "disable coverage on elaboration code - 1"); -- # disabled
   Z := Identity (3);                              -- # disabled
   pragma Annotate (Xcov, Cov_On);                 -- # disabled

   Another_Swap (X, Y);                            -- # elab

   pragma Annotate                                 -- # disabled
     (Xcov, Cov_Off,                               -- # disabled
      "disable coverage on elaboration code - 2"); -- # disabled
   Another_Swap (Z, X);                            -- # disabled
   pragma Annotate (Xcov, Cov_On);                 -- # disabled
end Disabled;
