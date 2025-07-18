with Ada.Environment_Variables;

with Logging;
with Outputs;
with Paths;        use Paths;
with Subprocesses; use Subprocesses;
with Support_Files;
with Temp_Dirs;    use Temp_Dirs;

package body LLVM is
   LLVM_Artifacts_Temp_Dir : Temporary_Directory;

   LLVM_Trace : constant Logging.GNATCOLL_Trace :=
     Logging.Create_Trace ("LLVM");

   function Get_LLVM_Profdata_Exe return String;
   --  Find the llvm-profdata from the `LLVM_PROFDATA_EXE` environment variable
   --  Or expect `llvm-prodata` to be on the PATH.

   function Get_LLVM_Trace_Adapter_Exe return String;
   --  Find the trace adapter binary, it should be in the libexec folder of
   --  gnatcov.

   function Make_Profdata_From_Traces
     (Traces :      Requested_Trace_Vectors.Vector;
      Output_File : String) return String;
   --  Builds the call to the `llvm-profdata` command, and saves the output
   --  profdata file in `Output_File`.
   --  Return the expanded the absolute path of `Output_File`.

   function Make_LLVM_Checkpoint_From_Profdata
     (Profdata_File, Exe_File, Output_File : String) return String;
   --  Builds the call to the LLVM trace adapter and saves the JSON result to
   --  `Output_File`.
   --  Return the expanded the absolute path of `Output_File`.

   ---------------------------
   -- Get_LLVM_Profdata_Exe --
   ---------------------------

   function Get_LLVM_Profdata_Exe return String is
      package EV renames Ada.Environment_Variables;
      Var_Name : constant String := "LLVM_PROFDATA_EXE";
   begin
      if EV.Exists (Var_Name) then
         return EV.Value (Var_Name);
      else
         return Support_Files.In_Libexec_Dir ("llvm-profdata");
      end if;
   end Get_LLVM_Profdata_Exe;

   --------------------------------
   -- Get_LLVM_Trace_Adapter_Exe --
   --------------------------------

   function Get_LLVM_Trace_Adapter_Exe return String
   is (Support_Files.In_Libexec_Dir ("gnatcov_llvm_exporter"));

   --------------------------
   -- Create_LLVM_Temp_Dir --
   --------------------------

   procedure Create_LLVM_Temp_Dir (Auto_Delete : Boolean) is
   begin
      Create_Temporary_Directory
        (LLVM_Artifacts_Temp_Dir, "gnatcov_llvm_artifacts", Auto_Delete);

      LLVM_Trace.Trace
        ("Created LLVM artifacts tempdir: "
         & LLVM_Artifacts_Temp_Dir.Directory_Name);
   end Create_LLVM_Temp_Dir;

   -------------------------------
   -- Make_Profdata_From_Traces --
   -------------------------------

   function Make_Profdata_From_Traces
     (Traces : Requested_Trace_Vectors.Vector; Output_File : String)
      return String
   is
      LLVM_Profdata_Cmd : Command_Type :=
        (+Get_LLVM_Profdata_Exe, others => <>);
      Output_Path       : constant String :=
        LLVM_Artifacts_Temp_Dir.Directory_Name / Output_File;
   begin
      Subprocesses.Append_Arg (LLVM_Profdata_Cmd, "merge");
      Subprocesses.Append_Arg (LLVM_Profdata_Cmd, "-sparse");
      Subprocesses.Append_Arg (LLVM_Profdata_Cmd, "-o", Output_Path);

      for RT of Traces loop
         Subprocesses.Append_Arg (LLVM_Profdata_Cmd, +RT.Filename);
      end loop;

      if not Subprocesses.Run_Command
               (Command             => LLVM_Profdata_Cmd,
                Origin_Command_Name => "gnatcov coverage",
                Err_To_Out          => True,
                In_To_Null          => True)
      then
         Outputs.Fatal_Error ("Call to `llvm-profdata` didn't succeed.");
      end if;

      return Output_Path;
   end Make_Profdata_From_Traces;

   ----------------------------------------
   -- Make_LLVM_Checkpoint_From_Profdata --
   ----------------------------------------

   function Make_LLVM_Checkpoint_From_Profdata
     (Profdata_File, Exe_File, Output_File : String) return String
   is
      Trace_Adapter_Cmd : Command_Type :=
        (+Get_LLVM_Trace_Adapter_Exe, others => <>);
   begin
      return Output_Path : constant String :=
        LLVM_Artifacts_Temp_Dir.Directory_Name / Output_File
      do
         Subprocesses.Append_Arg (Trace_Adapter_Cmd, "--instr-prof");
         Subprocesses.Append_Arg (Trace_Adapter_Cmd, Profdata_File);
         Subprocesses.Append_Arg (Trace_Adapter_Cmd, Output_Path);
         Subprocesses.Append_Arg (Trace_Adapter_Cmd, Exe_File);
         Subprocesses.Run_Command
           (Command             => Trace_Adapter_Cmd,
            Origin_Command_Name => "gnatcov coverage",
            In_To_Null          => True);
      end return;
   end Make_LLVM_Checkpoint_From_Profdata;

   --------------------------------------
   -- Make_LLVM_Checkpoint_From_Traces --
   --------------------------------------

   function Make_LLVM_Checkpoint_From_Traces
     (Trace_Inputs : Requested_Trace_Vectors.Vector;
      Exe_File     : String) return Unbounded_String
   is
      LLVM_Profdata_File : constant String := "output.profdata";
      LLVM_JSON_File     : constant String := "llvm-ckpt.json";

      LLVM_Profdata_Path : Unbounded_String;
      LLVM_JSON_Path     : Unbounded_String;
   begin

      --  Aggregate profraws using llvm-profdata

      LLVM_Profdata_Path :=
        +LLVM.Make_Profdata_From_Traces (Trace_Inputs, LLVM_Profdata_File);

      --  Convert the profdata into a JSON using trace adapter

      LLVM_JSON_Path :=
        +LLVM.Make_LLVM_Checkpoint_From_Profdata
           (+LLVM_Profdata_Path, Exe_File, LLVM_JSON_File);

      return LLVM_JSON_Path;

   end Make_LLVM_Checkpoint_From_Traces;

end LLVM;
