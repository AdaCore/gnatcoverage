with Ada.Environment_Variables;

with Outputs;
with Strings;      use Strings;
with Subprocesses; use Subprocesses;
with Support_Files;

package body LLVM is

   function Get_LLVM_Profdata_Exe return String;
   --  Find the llvm-profdata from the `LLVM_PROFDATA_EXE` environment variable
   --  Or expect `llvm-prodata` to be on the PATH.

   function Get_LLVM_Trace_Adapter_Exe return String;
   --  Find the trace adapter binary, it should be in the libexec folder of
   --  gnatcov.

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

   function Get_LLVM_Trace_Adapter_Exe return String is
      (Support_Files.In_Libexec_Dir ("gnatcov_llvm_exporter"));

   -------------------------------
   -- Make_Profdata_From_Traces --
   -------------------------------

   procedure Make_Profdata_From_Traces
     (Traces : Requested_Trace_Vectors.Vector; Output_File : String)
   is
      LLVM_Profdata_Cmd : Command_Type :=
        (+Get_LLVM_Profdata_Exe, others => <>);
   begin
      Subprocesses.Append_Arg (LLVM_Profdata_Cmd, "merge");
      Subprocesses.Append_Arg (LLVM_Profdata_Cmd, "-sparse");
      Subprocesses.Append_Arg (LLVM_Profdata_Cmd, "-o", Output_File);

      for RT of Traces loop
         Subprocesses.Append_Arg (LLVM_Profdata_Cmd, +RT.Filename);
      end loop;

      if not Subprocesses.Run_Command
        (Command             => LLVM_Profdata_Cmd,
         Origin_Command_Name => "gnatcov coverage",
         Err_To_Out          => True,
         In_To_Null          => True)
      then
         Outputs.Fatal_Error
           ("Call to `llvm-profdata` didn't succeed.");
      end if;
   end Make_Profdata_From_Traces;

   ----------------------------------------
   -- Make_LLVM_Checkpoint_From_Profdata --
   ----------------------------------------

   procedure Make_LLVM_Checkpoint_From_Profdata
     (Profdata_File, Exe_File, Output_File : String)
   is
      Trace_Adapter_Cmd : Command_Type :=
        (+Get_LLVM_Trace_Adapter_Exe, others => <>);
   begin

      Subprocesses.Append_Arg (Trace_Adapter_Cmd, "--instr-prof");
      Subprocesses.Append_Arg (Trace_Adapter_Cmd, Profdata_File);
      Subprocesses.Append_Arg (Trace_Adapter_Cmd, Exe_File);

      if not Subprocesses.Run_Command
        (Command             => Trace_Adapter_Cmd,
         Origin_Command_Name => "gnatcov coverage",
         Output_File         => Output_File,
         In_To_Null          => True)
      then
         Outputs.Fatal_Error
           ("Call to llvm trace adapter didn't succeed."
            & " See `" & Output_File & "` for details.");
      end if;
   end Make_LLVM_Checkpoint_From_Profdata;

end LLVM;
