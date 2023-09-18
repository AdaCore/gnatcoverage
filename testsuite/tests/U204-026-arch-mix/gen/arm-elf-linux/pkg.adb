pragma Style_Checks (Off); pragma Warnings (Off);
with Ada.Text_IO; use Ada.Text_IO;

with GNATcov_RTS.Buffers;with GCVRT.Pz3791e32c;package body Pkg is

   -------------
   -- Compute --
   -------------

   procedure Compute (A, B : Boolean) is
   MCDC_State_2_Var:GNATcov_RTS.Buffers.MCDC_State_Type:=0;MCDC_State_2:constant GNATCov_RTS.Sys.Address:=MCDC_State_2_Var'Address;begin
      GNATcov_RTS.Buffers.Witness(GCVRT.Pz3791e32c.Buffers_2.Statement_Buffer,0);if GNATcov_RTS.Buffers.Witness(GCVRT.Pz3791e32c.Buffers_2.Decision_Buffer,0,1,GCVRT.Pz3791e32c.Buffers_2.MCDC_Buffer,0,MCDC_State_2,GNATcov_RTS.Buffers.Witness(MCDC_State_2,1,TRUE,A )and then GNATcov_RTS.Buffers.Witness(MCDC_State_2,1,FALSE,B ))then
         GNATcov_RTS.Buffers.Witness(GCVRT.Pz3791e32c.Buffers_2.Statement_Buffer,1);Put_Line ("true");
      else
         GNATcov_RTS.Buffers.Witness(GCVRT.Pz3791e32c.Buffers_2.Statement_Buffer,2);Put_Line ("false");
      end if;
   end Compute;

end Pkg;

