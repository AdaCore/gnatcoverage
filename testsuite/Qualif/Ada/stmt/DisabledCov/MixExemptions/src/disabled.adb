package body Disabled is

   function Complex_Identity (X : Integer) return Integer is
   begin

      --  Nest a disable coverage region in an exemption region.

      pragma Annotate (Xcov, Exempt_On, "Exempted region");        -- # exempted
      if X = 0 then                                                -- # exempted
         return X;                                                 -- # exempted
      end if;                                                      -- # exempted
                                                                   -- # exempted
      pragma Annotate (Xcov, Cov_Off, "Disabled coverage region"); -- # disabled
      if X = 1 then                                                -- # disabled
         return X;                                                 -- # disabled
      end if;                                                      -- # disabled
      pragma Annotate (Xcov, Cov_On);                              -- # disabled
                                                                   -- # exempted
      if X = 2 then                                                -- # exempted
         return X;                                                 -- # exempted
      end if;                                                      -- # exempted
                                                                   -- # exempted
      --  End of the exemption region                              -- # exempted
                                                                   -- # exempted
      pragma Annotate (Xcov, Exempt_Off);                          -- # exempted

      --  Now do the opposite: nest the exemption region in the disable
      --  coverage region.

      pragma Annotate (Xcov, Cov_Off, "Disabled coverage region"); -- # disabled
                                                                   -- # disabled
      if X = 3 then                                                -- # disabled
         return X;                                                 -- # disabled
      end if;                                                      -- # disabled
                                                                   -- # disabled
      pragma Annotate (Xcov, Exempt_On, "Exempted region");        -- # dis-ex
      if X = 4 then                                                -- # dis-ex
         return X;                                                 -- # dis-ex
      end if;                                                      -- # dis-ex
      pragma Annotate (Xcov, Exempt_Off);                          -- # dis-ex
                                                                   -- # disabled
      return X;                                                    -- # disabled
                                                                   -- # disabled
      --  End the disabled coverage region                         -- # disabled
                                                                   -- # disabled
      pragma Annotate (Xcov, Cov_On);                              -- # disabled

   end Complex_Identity;
end Disabled;
