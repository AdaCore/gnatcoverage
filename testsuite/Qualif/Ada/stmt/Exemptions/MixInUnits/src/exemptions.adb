package body Exemptions is

   procedure Swap (I, J : in out Integer) is

      --  Exemption for a local declaration

      pragma Annotate                                   -- # xswap
        (Xcov, Exempt_On, "exemption on declarations"); -- # xswap
      Tmp : Integer := J;                               -- # xswap
      pragma Annotate (Xcov, Exempt_Off);               -- # xswap
   begin
      J := I;                                              -- # swap_stmt
      I := Tmp;                                            -- # swap_stmt
   end Swap;

   function Factorial (X : Natural) return Positive is
   begin
      if X = 1 then                                        -- # factorial
         return 1;                                         -- # 1_factorial
      elsif X >= Positive'Last / 1000 then                 -- # elsif_factorial

         --  Exemption for a conditioned set of statements

         pragma Annotate                                   -- # xfactorial
           (Xcov, Exempt_On,                               -- # xfactorial
              "exemption on statements in function");      -- # xfactorial
         return Positive'Last;                             -- # xfactorial
         pragma Annotate (Xcov, Exempt_Off);               -- # xfactorial
      else
         return X * Factorial (X - 1);                     -- # rec_factorial
      end if;
   end Factorial;

   --  No exemption in the couple of subprograms below

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

   --  Mix of exempted and non-exempted statements in the package
   --  elaboration sequence

   pragma Annotate                            -- # xelab_1
     (Xcov, Exempt_On,                        -- # xelab_1
        "exemption on elaboration code - 1"); -- # xelab_1
   Z := Identity (3);                         -- # xelab_1
   pragma Annotate (Xcov, Exempt_Off);        -- # xelab_1

   Another_Swap (X, Y);                       -- # elab

   pragma Annotate                            -- # xelab_2
     (Xcov, Exempt_On,                        -- # xelab_2
        "exemption on elaboration code - 2"); -- # xelab_2
   Another_Swap (Z, X);                       -- # xelab_2
   pragma Annotate (Xcov, Exempt_Off);        -- # xelab_2
end Exemptions;
