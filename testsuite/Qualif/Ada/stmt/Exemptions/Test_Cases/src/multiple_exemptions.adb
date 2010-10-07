--  This procedure performs completely meaningless computations, its goal is
--  to create a natural context for several exemption sections
procedure Multiple_Exemptions (I, J, K : in out Integer) is
   Tmp : Integer := I;                                -- # dcl
begin


   if I = 0 and then J = 0 and then K = 0 then       -- # 1_if
      pragma Annotate (Xcov, Exempt_On, "exemption section #1");
      raise Constraint_Error;                        -- # 1_exem
      pragma Annotate (Xcov, Exempt_Off);
   end if;


   I := (I + J + K) / 3;                             -- # stmt

   if I > 0 then                                     -- # stmt

      if J < 0 then                                  -- # 2_if
         pragma Annotate (Xcov, Exempt_On, "exemption section #2");
            J := -J;                                 -- # 2_exem
         pragma Annotate (Xcov, Exempt_Off);
      end if;

     I := Tmp;                                       -- # 2_if

   end if;

   J := J + K;                                       -- # stmt

   if K < 0 then                                     -- # stmt
      if J = 0 then                                  -- # 3_if
         pragma Annotate (Xcov, Exempt_On, "exemption section #3");
            J := 1;                                  -- # 3_exem
         pragma Annotate (Xcov, Exempt_Off);
      end if;

      K := K + I;                                    -- # 3_if
   end if;

exception
   when Constraint_Error =>
      pragma Annotate (Xcov, Exempt_On, "exemption section in handler");
      Tmp := I + J + K;                              -- # handler
      I := Tmp;                                      -- # handler
      J := Tmp + 1;                                  -- # handler
      K := Tmp + 2;                                  -- # handler
      pragma Annotate (Xcov, Exempt_Off);
end Multiple_Exemptions;
