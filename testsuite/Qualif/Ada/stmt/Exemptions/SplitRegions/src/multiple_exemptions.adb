--  This procedure performs completely meaningless computations, its goal is
--  to create a natural context for several exemption sections

with MX; use MX;

--  I, J, and K are functional parameters. X1, X2, X3 tell if we went
--  into the exempted regions 1, 2, and 3 respectively. Xh tells if we
--  went into the exempted handler.

procedure Multiple_Exemptions
  (I, J, K : in out Integer; Xf : access Xflags)
is
   Tmp : Integer := I;                                -- # dcl
begin

   Xf.X1 := False;  -- # stmt
   Xf.X2 := False;  -- # stmt
   Xf.X3 := False;  -- # stmt
   Xf.Xh := False;  -- # stmt

   if I = 0 and then J = 0 and then K = 0 then        -- # 1_if
      Xf.X1 := True;                                  -- # 1_flag
      pragma Annotate                                 -- # 1_exem
        (Xcov, Exempt_On, "exemption section #1");    -- # 1_exem
      raise Constraint_Error;                         -- # 1_exem
      pragma Annotate (Xcov, Exempt_Off);             -- # 1_exem
   end if;

   I := (I + J + K) / 3;                              -- # stmt
   if I > 0 then                                      -- # stmt
      if J < 0 then                                   -- # 2_if
         Xf.X2 := True;                               -- # 2_flag
         pragma Annotate                              -- # 2_exem
           (Xcov, Exempt_On, "exemption section #2"); -- # 2_exem
         J := -J;                                     -- # 2_exem
         pragma Annotate (Xcov, Exempt_Off);          -- # 2_exem
      end if;
      I := Tmp;                                       -- # 2_if
   end if;

   J := J + K;                                        -- # stmt
   if K < 0 then                                      -- # stmt
      if J = 0 then                                   -- # 3_if
         Xf.X3 := True;                               -- # 3_flag
         pragma Annotate                              -- # 3_exem
           (Xcov, Exempt_On, "exemption section #3"); -- # 3_exem
         J := 1;                                      -- # 3_exem
         pragma Annotate (Xcov, Exempt_Off);          -- # 3_exem
      end if;
      K := K + I;                                    -- # 3_if
   end if;

exception
   when Constraint_Error =>
      Xf.Xh := True;                                        -- # h_flag
      pragma Annotate                                       -- # h_exem
        (Xcov, Exempt_On, "exemption section in handler");  -- # h_exem
      Tmp := I + J + K;                                     -- # h_exem
      I := Tmp;                                             -- # h_exem
      J := Tmp + 1;                                         -- # h_exem
      K := Tmp + 2;                                         -- # h_exem
      pragma Annotate (Xcov, Exempt_Off);                   -- # h_exem
end Multiple_Exemptions;
