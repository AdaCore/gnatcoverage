pragma Ada_2012;

pragma Annotate (Xcov, Cov_Off, "Useless unit");  -- # cov
                                                  -- # cov
package Selective is                              -- # cov
                                                  -- # cov
   procedure Bar is null;                         -- # cov
                                                  -- # cov
end Selective;                                    -- # cov
pragma Annotate (Xcov, Cov_On);                   -- # cov
