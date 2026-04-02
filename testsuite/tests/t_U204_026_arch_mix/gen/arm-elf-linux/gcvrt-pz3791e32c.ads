pragma Style_Checks (Off); pragma Warnings (Off);

with System;
with GNATcov_RTS;
with GNATcov_RTS.Buffers;
pragma Compile_Time_Error (GNATcov_RTS.Version /= 12 ,"Incompatible GNATcov_RTS version, please use the GNATcov_RTS project provided with your GNATcoverage distribution.");

package GCVRT.Pz3791e32c is
   pragma Pure;

   package Buffers_1 is
      Statement_Buffer : constant System.Address;
      pragma Import (C, Statement_Buffer, "xcov__buf_stmt__z64144289_1");

      Decision_Buffer : constant System.Address;
      pragma Import (C, Decision_Buffer, "xcov__buf_dc__z64144289_1");

      MCDC_Buffer : constant System.Address;
      pragma Import (C, MCDC_Buffer, "xcov__buf_mcdc__z64144289_1");
   end Buffers_1;
   package Buffers_2 is
      Statement_Buffer : constant System.Address;
      pragma Import (C, Statement_Buffer, "xcov__buf_stmt__z64144278_2");

      Decision_Buffer : constant System.Address;
      pragma Import (C, Decision_Buffer, "xcov__buf_dc__z64144278_2");

      MCDC_Buffer : constant System.Address;
      pragma Import (C, MCDC_Buffer, "xcov__buf_mcdc__z64144278_2");
   end Buffers_2;


end GCVRT.Pz3791e32c;
