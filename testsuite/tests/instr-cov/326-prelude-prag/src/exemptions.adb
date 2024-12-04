pragma Annotate (Xcov, Exempt_On, "Useless unit");  -- # exempt
                                                    -- # exempt
package body Exemptions is                          -- # exempt
                                                    -- # exempt
   procedure Foo is null;                           -- # exempt_st
                                                    -- # exempt
end Exemptions;                                     -- # exempt
pragma Annotate (Xcov, Exempt_Off);                 -- # exempt
