with Traces_Files; use Traces_Files;
with Strings;      use Strings;

package LLVM is

   procedure Create_LLVM_Temp_Dir (Auto_Delete : Boolean);

   function Make_LLVM_Checkpoint_From_Traces
     (Trace_Inputs : Requested_Trace_Vectors.Vector;
      Exe_File     : String) return Unbounded_String;
   --  Use `llvm-profdata` and the gnatcov LLVM trace adapter to convert the
   --  trace inputs into an LLVM JSON checkpoint.
   --  Return the expanded the absolute path of the JSON file.

end LLVM;
