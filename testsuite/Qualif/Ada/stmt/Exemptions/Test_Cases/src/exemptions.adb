package body Exemptions is

   procedure Swap (I, J : in out Integer) is
      pragma Annotate (Xcov, Exempt_On, "exemption on declarations");
         Tmp : Integer := J;                               -- # swap_decl
      pragma Annotate (Xcov, Exempt_Off);
   begin
      J := I;                                              -- # swap_stmt
      I := Tmp;                                            -- # swap_stmt
   end Swap;

   function Factorial (X : Natural) return Positive is
   begin
      if X = 1 then                                        -- # factorial
         return 1;                                         -- # 1_factorial
      elsif X >= Positive'Last / 1000 then                 -- # elsif_factorial
         pragma Annotate (Xcov, Exempt_On, "exemption on statements in function");
         return Positive'Last;                             -- # ex_factorial
         pragma Annotate (Xcov, Exempt_Off);
      else
         return X * Factorial (X - 1);                     -- # rec_factorial
      end if;
   end Factorial;

   procedure Another_Swap (I, J : in out Integer) is
      Tmp : Integer := J;                                  -- # another_swap
   begin
      J := I;                                              -- # anothe_swap
      I := Tmp;                                            -- # anothe_swap
   end Another_Swap;

   function Another_Factorial (X : Natural) return Positive is
      Result : Positive := 1;                              -- # another_factorial
   begin
      for J in 1 .. X loop                                 -- # another_factorial
         if Result > Positive'Last / J then                -- # in_loop_a_f
            raise Constraint_Error;                        -- # in_if_a_f
         end if;

         Result := Result * J;                             -- # in_loop_a_f
      end loop;

      return Result;                                       -- # another_factorial
   exception
      when Constraint_Error =>
         return Positive'Last;                             -- # handler_a_f
   end Another_Factorial;


begin
   pragma Annotate (Xcov, Exempt_On, "exemption on elaboration code - 1");
   Z := Identity (3);                                      -- # 1_elab
   pragma Annotate (Xcov, Exempt_Off);

   Another_Swap (X, Y);                                    -- # elab

   pragma Annotate (Xcov, Exempt_On, "exemption on elaboration code - 2");
   Another_Swap (Z, X);                                    -- # 2_elab
   pragma Annotate (Xcov, Exempt_Off);
end Exemptions;
