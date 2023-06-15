pragma Style_Checks (Off); pragma Warnings (Off);
with Pkg;

with GNATcov_RTS.Buffers;with GCVRT.Pmain_1;with GCVRT.DB_main_1;with GNATcov_RTS;with GNATcov_RTS.Buffers;pragma Compile_Time_Error(GNATcov_RTS.Version/=5,"Incompatible GNATcov_RTS version, please use the GNATcov_RTS project provided with your GNATcoverage distribution.");procedure Main_1 is
begin
   GNATcov_RTS.Buffers.Witness(GCVRT.Pmain_1.Buffers_1.Statement_Buffer,0);Pkg.Compute (True, False);
GCVRT.DB_main_1.Dump_Buffers;end Main_1;


