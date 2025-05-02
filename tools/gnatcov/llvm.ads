with Traces_Files; use Traces_Files;

package LLVM is

   procedure Make_Profdata_From_Traces
     (Traces : Requested_Trace_Vectors.Vector; Output_File : String);
   --  Builds the call to the `llvm-profdata` command, and saves the output
   --  profdata file in `Output_File`.

   procedure Make_LLVM_Checkpoint_From_Profdata
     (Profdata_File, Exe_File, Output_File : String);
   --  Builds the call to the LLVM trace adapter and saves the JSON result to
   --  `Output_File`.

end LLVM;
