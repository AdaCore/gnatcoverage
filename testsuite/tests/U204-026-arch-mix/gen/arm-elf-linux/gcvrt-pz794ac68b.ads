pragma Style_Checks (Off); pragma Warnings (Off);
with System;
with GNATcov_RTS;
with GNATcov_RTS.Buffers;
pragma Compile_Time_Error (GNATcov_RTS.Version /= 9 ,"Incompatible GNATcov_RTS version, please use the GNATcov_RTS project provided with your GNATcoverage distribution.");

package GCVRT.Pz794ac68b is

   pragma Pure;

package Buffers_1 is
   Statement_Buffer : constant System.Address;
   pragma Import (C, Statement_Buffer, "xcov__buf_stmt__B_z794ac68b_1");

   Decision_Buffer : constant System.Address;
   pragma Import (C, Decision_Buffer, "xcov__buf_dc__B_z794ac68b_1");

   MCDC_Buffer : constant System.Address;
   pragma Import (C, MCDC_Buffer, "xcov__buf_mcdc__B_z794ac68b_1");

end Buffers_1;

end GCVRT.Pz794ac68b;
